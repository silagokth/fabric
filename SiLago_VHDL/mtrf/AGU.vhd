-------------------------------------------------------
--! @file
--! @brief AGU
-------------------------------------------------------
---------------- Copyright (c) notice -----------------------------------------
--
-- The VHDL code, the logic and concepts described in this file constitute
-- the intellectual property of the authors listed below, who are affiliated
-- to KTH(Kungliga Tekniska Högskolan), School of ICT, Kista.
-- Any unauthorised use, copy or distribution is strictly prohibited.
-- Any authorised use, copy or distribution should carry this copyright notice
-- unaltered.
-------------------------------------------------------------------------------
-- Title      : bus_selector
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : AGU.vhd
-- Author     : Nasim Farahini
-- Company    : KTH
-- Created    : 
-- Last update: 2013-09-10
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2013-09-10  1.0      Nasim Farahini  Created  -- Covers implementation of one level affine loop, 
-- 2014-02-15  2.0      Nasim Farahini  Modified -- Covers Repetition, Repetition Delay, Middle Delay
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	     #
--    General Public License as published by the Free Software Foundation, #
--    either version 3 of the License, or (at your option) any             #
--    later version.                                                       #
--                                                                         #
--    SiLago is distributed in the hope that it will be useful,            #
--    but WITHOUT ANY WARRANTY; without even the implied warranty of       #
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        #
--    GNU General Public License for more details.                         #
--                                                                         #
--    You should have received a copy of the GNU General Public License    #
--    along with SiLago.  If not, see <https://www.gnu.org/licenses/>.     #
--                                                                         #
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

LIBRARY ieee, work;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE ieee.std_logic_unsigned.ALL;
USE work.top_consts_types_package.ALL;

ENTITY AGU IS
  
  PORT (
    clk                 : IN  std_logic;
    rst_n               : IN  std_logic;
    instr_start         : IN  std_logic;
    instr_initial_delay : IN  std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
    instr_start_addrs   : IN  std_logic_vector(START_ADDR_WIDTH - 1 DOWNTO 0);
    instr_step_val      : IN  std_logic_vector(ADDR_OFFSET_WIDTH - 1 DOWNTO 0);
    instr_step_val_sign : IN  std_logic_vector(ADDR_OFFSET_SIGN_WIDTH - 1 DOWNTO 0);
    instr_no_of_addrs   : IN  std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
    instr_middle_delay  : IN std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE-1 DOWNTO 0);
    instr_no_of_rpts    : IN std_logic_vector(NUM_OF_REPT_PORT_SIZE-1 DOWNTO 0);
    instr_rpt_step_value    : IN std_logic_vector(REP_STEP_VALUE_PORT_SIZE-1 DOWNTO 0);
    instr_rpt_delay  : IN std_logic_vector(REPT_DELAY_VECTOR_SIZE-1 DOWNTO 0);
    instr_mode       : IN std_logic_vector(MODE_SEL_VECTOR_SIZE-1 DOWNTO 0);
    instr_fft_stage  : IN std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE-1 DOWNTO 0);
    instr_end_fft_stage   : IN std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE-1 DOWNTO 0);
    addr_out            : OUT std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
    addr_en             : OUT std_logic
    --all_done            : OUT std_logic
    );

END AGU;

ARCHITECTURE behave OF AGU IS

  TYPE   state_type IS (IDLE_ST, INITIAL_DELAY_ST, LINEAR_ST, RPT_DELAY_ST , RPT_ST, FFT_ST);

  SIGNAL pres_state, next_state                                                                  : state_type;
  SIGNAL delay_counter                                                                           : std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
  SIGNAL  addr_counter                                                                          : std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
  SIGNAL one_addr , delay_count_en , addr_count_en, addr_value_en  , init_zero , all_done_temp, no_more_rpt, add_sub_addr_en : std_logic;
  SIGNAL rpt_no_count_en ,rpt_start_addrs_en, rpt_delay_count_en, middle_delay_flag, middle_delay_first_cycle, middle_delay_count_en, addr_count_halt_en: std_logic;
  SIGNAL step_val_reg  , step_val_temp_in  : std_logic_vector(ADDR_OFFSET_WIDTH - 1 DOWNTO 0);
  SIGNAL no_of_addrs_reg                                                                         : std_logic_vector(ADDR_COUNTER_WIDTH - 1 DOWNTO 0);
  SIGNAL start_addrs_reg , rpt_start_addrs_reg_in, rpt_start_addrs_reg  ,start_addr_fft          : std_logic_vector(START_ADDR_WIDTH - 1 DOWNTO 0);
  SIGNAL initial_delay_reg                                                                       : std_logic_vector(INITIAL_DELAY_WIDTH - 1 DOWNTO 0);
  SIGNAL rpt_no_counter                                                                           : std_logic_vector(5  DOWNTO 0);
  signal middle_delay_reg, middle_delay_counter :  std_logic_vector(REG_FILE_MIDDLE_DELAY_PORT_SIZE-1 DOWNTO 0);
  signal no_of_rpts_reg        :  std_logic_vector(NUM_OF_REPT_PORT_SIZE-1 DOWNTO 0);
  signal rpt_step_value_reg    :  std_logic_vector(REP_STEP_VALUE_PORT_SIZE-1 DOWNTO 0);
  signal rpt_delay_reg,rpt_delay_counter   :  std_logic_vector(REPT_DELAY_VECTOR_SIZE-1 DOWNTO 0);
  signal step_val_sign_reg , add_sub_addr     :  std_logic_vector(ADDR_OFFSET_SIGN_WIDTH - 1 DOWNTO 0);
  signal fft_stage_reg, fft_stage ,end_fft_stage_reg :  std_logic_vector(FFT_STAGE_SEL_VECTOR_SIZE-1 DOWNTO 0);
  signal mode_reg       : std_logic_vector(MODE_SEL_VECTOR_SIZE-1 DOWNTO 0);
  signal first_fft_addr_out ,fft_addr_temp_out  ,addr_temp_in,addr_temp_out   : std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
  signal no_more_fft_stage ,fft_stage_count_en,one_fft_stage_over  : std_logic;
  
BEGIN

one_addr <= '1' when instr_no_of_addrs = "000000" else '0';
init_zero <= '1' when (instr_start = '1' and instr_initial_delay= "0000") else '0';
rpt_start_addrs_reg_in <= start_addrs_reg when rpt_no_counter="000000" else rpt_start_addrs_reg;
no_more_rpt <= '1' when rpt_no_counter= no_of_rpts_reg else '0';  
middle_delay_flag <= '0' when middle_delay_counter= middle_delay_reg else '1';
middle_delay_first_cycle <= '0' when middle_delay_counter="000000" else '1';
add_sub_addr_en <= '0' when add_sub_addr="0" else '1';
start_addr_fft <=  instr_start_addrs when init_zero='1' else start_addrs_reg;
fft_stage <= instr_fft_stage when init_zero='1' else fft_stage_reg;
no_more_fft_stage <= '1' when fft_stage_reg = end_fft_stage_reg else '0';

  reg_input_param : PROCESS (clk, rst_n) IS
  BEGIN  
    IF rst_n = '0' THEN      
      step_val_reg      <= (OTHERS => '0');
      step_val_sign_reg      <= (OTHERS => '0');
      no_of_addrs_reg   <= (OTHERS => '0');
      start_addrs_reg   <= (OTHERS => '0');
      initial_delay_reg <= (OTHERS => '0');
      middle_delay_reg <= (OTHERS => '0');
      no_of_rpts_reg        <= (OTHERS => '0');
      rpt_step_value_reg   <= (OTHERS => '0');
      rpt_delay_reg <= (OTHERS => '0');
      end_fft_stage_reg <= (OTHERS => '0');
      mode_reg <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN 
      IF instr_start = '1' THEN
        step_val_reg      <= instr_step_val;
        step_val_sign_reg      <= instr_step_val_sign;        
        no_of_addrs_reg   <= instr_no_of_addrs;
        start_addrs_reg   <= instr_start_addrs;
        initial_delay_reg <= instr_initial_delay;
        middle_delay_reg <= instr_middle_delay;
        no_of_rpts_reg        <= instr_no_of_rpts;
        rpt_step_value_reg   <= instr_rpt_step_value;
        rpt_delay_reg <= instr_rpt_delay;
        end_fft_stage_reg <=  instr_end_fft_stage;
        mode_reg <= instr_mode;
      ELSIF all_done_temp = '1' THEN
        step_val_reg      <= (OTHERS => '0');
        step_val_sign_reg      <= (OTHERS => '0');
        no_of_addrs_reg   <= (OTHERS => '0');
        start_addrs_reg   <= (OTHERS => '0');
        initial_delay_reg <= (OTHERS => '0');
        middle_delay_reg <= (OTHERS => '0');
        no_of_rpts_reg        <= (OTHERS => '0');
        rpt_step_value_reg   <= (OTHERS => '0');
        rpt_delay_reg <= (OTHERS => '0');
        end_fft_stage_reg <= (OTHERS => '0');
        mode_reg <= (OTHERS => '0');
      END IF;
    END IF;
  END PROCESS reg_input_param;



  init_del_cnt : PROCESS (clk, rst_n) IS
  BEGIN
    IF rst_n = '0' THEN
      delay_counter <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN
      IF delay_count_en = '1' THEN
        delay_counter <= delay_counter + 1;
      ELSE
        delay_counter <= (OTHERS => '0');
      END IF;
    END IF;
  END PROCESS init_del_cnt;


  rpt_del_cnt : PROCESS (clk, rst_n) IS
  BEGIN
    IF rst_n = '0' THEN
      rpt_delay_counter <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN
      IF rpt_delay_count_en = '1' THEN
        rpt_delay_counter <= rpt_delay_counter + 1;
      ELSE
        rpt_delay_counter <= (OTHERS => '0');
      END IF;
    END IF;
  END PROCESS rpt_del_cnt;
  
  
  mdl_del_cnt : PROCESS (clk, rst_n) IS
  BEGIN
    IF rst_n = '0' THEN
      middle_delay_counter <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN
      IF middle_delay_count_en = '1' THEN
        middle_delay_counter <= middle_delay_counter + 1;
      ELSE
        middle_delay_counter <= (OTHERS => '0');
      END IF;
    END IF;
  END PROCESS mdl_del_cnt;
  
  
  rpt_no_cnt : PROCESS (clk, rst_n) IS
  BEGIN
    IF rst_n = '0' THEN
      rpt_no_counter <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN
      IF rpt_no_count_en = '1' THEN
        rpt_no_counter <= rpt_no_counter + 1;
      elsif all_done_temp='1'or one_fft_stage_over='1' then
      	rpt_no_counter <= (OTHERS => '0');
      END IF;
    END IF;
  END PROCESS rpt_no_cnt;


  addr_cnt : PROCESS (clk, rst_n)
  BEGIN  
    IF rst_n = '0' THEN                
      addr_counter  <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN  
      IF addr_count_en = '1' and addr_count_halt_en = '0'THEN
        addr_counter <= addr_counter + 1;
      ELSIF addr_count_en = '0' then 
        addr_counter  <= (OTHERS => '0');
      END IF;
    END IF;
  END PROCESS addr_cnt;
  
  
  rpt_start_addrs_value : PROCESS (clk, rst_n)
  BEGIN  
    IF rst_n = '0' THEN                
      rpt_start_addrs_reg <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN  
      IF rpt_start_addrs_en = '1' THEN
        rpt_start_addrs_reg <= rpt_start_addrs_reg_in + rpt_step_value_reg;
      END IF;
    END IF;
  END PROCESS rpt_start_addrs_value;
  
  
  addr_value : PROCESS(clk, rst_n)
  BEGIN
  	IF rst_n = '0' THEN
  		addr_temp_out <= (OTHERS => '0');
  	ELSIF clk'event AND clk = '1' THEN
  		IF addr_value_en = '1' THEN
  			if add_sub_addr_en = '0' then
  				addr_temp_out <= addr_temp_in + step_val_temp_in;
  			else
  				addr_temp_out <= addr_temp_in - step_val_temp_in;
  			end if;
  		ELSE
  			addr_temp_out <= (OTHERS => '0');
  		END IF;
  	END IF;
  END PROCESS addr_value;

  fft_stage_cnt : PROCESS (clk, rst_n)
  BEGIN  
    IF rst_n = '0' THEN                
      fft_stage_reg  <= (OTHERS => '0');
    ELSIF clk'event AND clk = '1' THEN  
      if instr_mode ="1" and instr_start='1' then 
        fft_stage_reg <= instr_fft_stage;
      elsif fft_stage_count_en = '1' THEN
        fft_stage_reg <= fft_stage_reg + 1;
      ELSIF all_done_temp='1'  then
        fft_stage_reg  <= (OTHERS => '0');
      END IF;
    END IF;
  END PROCESS fft_stage_cnt;

  Bit_Rev_Address:process(fft_stage,addr_temp_out,start_addr_fft)
  begin

  	case fft_stage is
  		when "001" =>
  			first_fft_addr_out    <= start_addr_fft(0)&"00000";
  			fft_addr_temp_out     <= addr_temp_out(0) & addr_temp_out(1) & addr_temp_out(2) & addr_temp_out(3) & addr_temp_out(4) & addr_temp_out(5);
  		when "010" =>
  			fft_addr_temp_out     <= addr_temp_out(5) & addr_temp_out(0) & addr_temp_out(1) & addr_temp_out(2) & addr_temp_out(3) & addr_temp_out(4);
  		    first_fft_addr_out   <= '0'& start_addr_fft(0)&"0000";
  		when "011" =>
  			fft_addr_temp_out     <= addr_temp_out(5) & addr_temp_out(4) & addr_temp_out(0) & addr_temp_out(1) & addr_temp_out(2) & addr_temp_out(3);
  		    first_fft_addr_out    <= "00"& start_addr_fft(0)&"000";
  		when "100" =>
  			fft_addr_temp_out     <= addr_temp_out(5) & addr_temp_out(4) & addr_temp_out(3) & addr_temp_out(0) & addr_temp_out(1) & addr_temp_out(2);
  		    first_fft_addr_out   <= "000"& start_addr_fft(0)&"00";
  		when "101" =>
  			fft_addr_temp_out      <= addr_temp_out(5) & addr_temp_out(4) & addr_temp_out(3) & addr_temp_out(2) & addr_temp_out(0) & addr_temp_out(1);
  		    first_fft_addr_out    <= "0000" & start_addr_fft(0)&'0';
  		when others =>
  			fft_addr_temp_out      <= addr_temp_out(5) & addr_temp_out(4) & addr_temp_out(3) & addr_temp_out(2) & addr_temp_out(1) & addr_temp_out(0);
  	    	first_fft_addr_out    <= "00000" & start_addr_fft(0);
  	end case;
  end process;
  
  
  

  AGU_FSM : PROCESS (pres_state, middle_delay_flag,middle_delay_first_cycle, no_more_fft_stage,rpt_start_addrs_en ,no_more_rpt, rpt_no_count_en, instr_step_val, instr_start_addrs, instr_no_of_addrs, instr_start, instr_initial_delay, one_addr, addr_counter, start_addrs_reg, 
  	initial_delay_reg, delay_counter, addr_temp_out ,mode_reg, step_val_sign_reg,no_of_addrs_reg, rpt_delay_reg, rpt_delay_counter, instr_middle_delay, instr_no_of_rpts, instr_mode,instr_step_val_sign, instr_rpt_delay,step_val_reg,instr_rpt_step_value,no_of_rpts_reg,rpt_step_value_reg,rpt_start_addrs_reg, fft_addr_temp_out,first_fft_addr_out)--add instr_values
  BEGIN
    next_state     <= pres_state;
    addr_count_en  <= '0';
    addr_out       <= (OTHERS => '0');
    addr_en        <= '0';
    all_done_temp  <= '0';
    addr_temp_in   <= (OTHERS => '0');
    delay_count_en <= '0';
    step_val_temp_in <= step_val_reg;
    rpt_no_count_en <= '0';
    addr_value_en  <= '0';
    rpt_start_addrs_en <= '0';
    rpt_delay_count_en <= '0';
    middle_delay_count_en <= '0';
    addr_count_halt_en <= '0';
    add_sub_addr <= step_val_sign_reg;
    fft_stage_count_en  <= '0';
    one_fft_stage_over <= '0';
 

    CASE pres_state IS
      WHEN IDLE_ST =>
      addr_en    <= '0';
      next_state <= IDLE_ST;

      IF instr_start = '1' THEN
      	IF instr_initial_delay = "0000" THEN
      		if instr_middle_delay = "000000" then
      			if instr_mode = "0" then
      				step_val_temp_in <= instr_step_val;
      				add_sub_addr     <= instr_step_val_sign;
      			else
      				step_val_temp_in <= "000010";
      				add_sub_addr     <= "0";
      			end if;
      			if one_addr = '1' THEN
      				if instr_no_of_rpts = "000000" then
      					next_state    <= IDLE_ST;
      					addr_en       <= '1';
      					all_done_temp <= '1';
      					if instr_mode = "0" then
      						addr_out <= instr_start_addrs;
      					else
      						addr_out <= first_fft_addr_out;
      					end if;
      				else
      					if instr_rpt_delay = "000000" then
      						addr_temp_in       <= instr_start_addrs;
      						add_sub_addr       <= (OTHERS => '0');
      						rpt_no_count_en    <= '1';
      						rpt_start_addrs_en <= '1';
      						addr_en            <= '1';
      						addr_count_en      <= '0';
      						addr_value_en      <= '1';
      						if instr_mode = "0" then
      							next_state       <= LINEAR_ST;
      							addr_out         <= instr_start_addrs;
      							step_val_temp_in <= instr_rpt_step_value;
      						else      --fft
      							next_state       <= FFT_ST;
      							addr_out         <= first_fft_addr_out;
      							step_val_temp_in <= "000010";
      						end if;
      					else
      						next_state         <= RPT_DELAY_ST;
      						rpt_no_count_en    <= '1';
      						rpt_start_addrs_en <= '1';
      						rpt_delay_count_en <= '0';
      						addr_value_en      <= '1';
      						step_val_temp_in   <= (OTHERS => '0');
      						add_sub_addr       <= (OTHERS => '0');
      						addr_count_en      <= '1';
      						addr_en            <= '1';
      						addr_temp_in       <= instr_start_addrs;
      						if instr_mode = "0" then
      							addr_out <= instr_start_addrs;
      						else      --fft
      							addr_out <= first_fft_addr_out;
      						end if;
      					end if;
      				end if;
      			ELSE
      				addr_temp_in  <= instr_start_addrs;
      				addr_en       <= '1';
      				addr_count_en <= '1';
      				addr_value_en <= '1';
      				if instr_mode = "0" then
      					next_state   <= LINEAR_ST;
      					addr_out     <= instr_start_addrs;
      					add_sub_addr <= instr_step_val_sign;
      				else
      					next_state   <= FFT_ST;
      					addr_out     <= first_fft_addr_out;
      					add_sub_addr <= (OTHERS => '0');
      				end if;

      			END IF;
      		else
      			middle_delay_count_en <= '1';
      			addr_count_halt_en    <= '1';
      			step_val_temp_in      <= (OTHERS => '0');
      			add_sub_addr          <= (OTHERS => '0');
      			addr_temp_in          <= instr_start_addrs;
      			addr_en               <= '1';
      			addr_count_en         <= '1';
      			addr_value_en         <= '1';
      			if instr_mode = "0" then
      				next_state <= LINEAR_ST;
      				addr_out   <= instr_start_addrs;
      			else
      				next_state <= FFT_ST;
      				addr_out   <= first_fft_addr_out;
      			end if;
      		end if;
      	ELSE
      		IF instr_initial_delay = "0001" THEN
      			step_val_temp_in <= (OTHERS => '0');
      			add_sub_addr     <= (OTHERS => '0');
      			addr_temp_in     <= instr_start_addrs;
      			addr_en          <= '0';
      			addr_count_en    <= '0';
      			addr_value_en    <= '1';
      			if instr_mode = "0" then
      				next_state <= LINEAR_ST;
      			else
      				next_state <= FFT_ST;
      			end if;
      		else
      			next_state     <= INITIAL_DELAY_ST;
      			addr_en        <= '0';
      			delay_count_en <= '1';
      			addr_value_en  <= '0';
      			addr_count_en  <= '0';
      		end if;

      	END IF;


      END IF;

      WHEN INITIAL_DELAY_ST =>
      next_state     <= INITIAL_DELAY_ST;
      delay_count_en <= '1';
      IF delay_counter = initial_delay_reg - 1 THEN
      	delay_count_en   <= '0';
      	addr_en          <= '0';
      	addr_count_en    <= '0';
      	addr_value_en    <= '1';
      	step_val_temp_in <= (OTHERS => '0');
      	addr_temp_in     <= start_addrs_reg;
      	if mode_reg = "0" then
      		next_state <= LINEAR_ST;
      	else
      		next_state <= FFT_ST;
      	end if;

      END IF;

      WHEN LINEAR_ST =>
      addr_count_en  <= '1';
      addr_value_en  <= '1';
      delay_count_en <= '0';
      addr_out       <= addr_temp_out;
      add_sub_addr   <= step_val_sign_reg;
      addr_temp_in   <= addr_temp_out;
      next_state     <= LINEAR_ST;
      if middle_delay_first_cycle='0' then --Remove this condition if you need adr_en=1 during the middle_delay
          addr_en        <= '1';
      else 
          addr_en        <= '0';
      end if;       

      IF addr_counter = no_of_addrs_reg THEN
    		addr_count_en <= '0';
    		addr_value_en <= '1';

    		IF no_of_rpts_reg = "000000" or no_more_rpt = '1' then
    			next_state    <= IDLE_ST;
    			all_done_temp <= '1';
    		else
    			rpt_no_count_en    <= '1';
    			rpt_start_addrs_en <= '1';
    			if rpt_delay_reg = "000000" then
    				if no_of_addrs_reg = "000000" then
    					next_state       <= LINEAR_ST;
    					step_val_temp_in <= rpt_step_value_reg;
    					add_sub_addr     <= (OTHERS => '0');
    				else
    					next_state <= RPT_ST;
    				end if;
    			else
    				next_state         <= RPT_DELAY_ST;
    				rpt_delay_count_en <= '0';
    				addr_value_en      <= '1';
    				step_val_temp_in   <= (OTHERS => '0');
    				add_sub_addr       <= (OTHERS => '0');

    			end if;
    		end if;

      ELSE
        if middle_delay_flag = '1' then
        	addr_count_halt_en    <= '1';
        	step_val_temp_in      <= (OTHERS => '0');
        	add_sub_addr          <= (OTHERS => '0');
        	middle_delay_count_en <= '1';
     
        end if;
      END IF; 

      WHEN FFT_ST =>
      addr_count_en    <= '1';
      addr_value_en    <= '1';
      delay_count_en   <= '0';
      addr_out         <= fft_addr_temp_out;
      step_val_temp_in <= "000010";
      add_sub_addr     <= "0";
      addr_temp_in     <= addr_temp_out;
      next_state       <= FFT_ST;
      if middle_delay_first_cycle='0' then --Remove this condition if you need adr_en=1 during the middle_delay
          addr_en        <= '1';
      else 
          addr_en        <= '0';
      end if; 

      IF addr_counter = no_of_addrs_reg THEN
    		addr_count_en <= '0';
    		addr_value_en <= '1';
    		IF no_of_rpts_reg = "000000" or no_more_rpt = '1' then
    			if no_more_fft_stage ='1' then
    				next_state    <= IDLE_ST;
    				all_done_temp <= '1';
    			else
    				fft_stage_count_en  <= '1';
    				rpt_delay_count_en <= '0';
    				middle_delay_count_en <= '0';
    				one_fft_stage_over <= '1';
    				addr_count_en <= '0';
    				addr_value_en <= '1';
    				next_state    <= FFT_ST;
    				step_val_temp_in <= "000000";
    				addr_temp_in     <= start_addrs_reg;
    			end if;
    		else
    			rpt_no_count_en    <= '1';
    			rpt_start_addrs_en <= '1';
    			if rpt_delay_reg = "000000" then
    				if no_of_addrs_reg = "000000" then
    					next_state       <= LINEAR_ST;
    					step_val_temp_in <= rpt_step_value_reg;
    					add_sub_addr     <= (OTHERS => '0');
    				else
    					next_state <= RPT_ST;
    				end if;
    			else
    				next_state         <= RPT_DELAY_ST;
    				rpt_delay_count_en <= '0';
    				addr_value_en      <= '1';
    				step_val_temp_in   <= (OTHERS => '0');
    				add_sub_addr       <= (OTHERS => '0');
    			end if;
    		end if;
      ELSE
        if middle_delay_flag = '1' then
          addr_count_halt_en    <= '1';
          step_val_temp_in      <= (OTHERS => '0');
          add_sub_addr          <= (OTHERS => '0');
          middle_delay_count_en <= '1';
        end if;
      END IF;

      WHEN RPT_DELAY_ST =>
      next_state         <= RPT_DELAY_ST;
      rpt_delay_count_en <= '1';
      addr_temp_in       <= addr_temp_out;
      step_val_temp_in   <= (OTHERS => '0');
      add_sub_addr       <= (OTHERS => '0');
      addr_value_en      <= '1';
      IF rpt_delay_counter = rpt_delay_reg - 1 THEN
      	rpt_delay_count_en <= '0';
      	if no_of_addrs_reg = "000000" then
      		if mode_reg = "0" then
      			next_state       <= LINEAR_ST;
      			step_val_temp_in <= rpt_step_value_reg;
      		else
      			next_state       <= FFT_ST;
      			step_val_temp_in <= "000000";
      		end if;

      		add_sub_addr <= (OTHERS => '0');
      	else
      		next_state <= RPT_ST;
      	end if;
      END IF;

      WHEN RPT_ST =>
      addr_count_en  <= '1';
      addr_value_en  <= '1';
      delay_count_en <= '0';
      if mode_reg = "0" then
      	addr_out     <= rpt_start_addrs_reg;
      	next_state   <= LINEAR_ST;
      	addr_temp_in <= rpt_start_addrs_reg;
      else
      	addr_out     <= first_fft_addr_out;
      	next_state   <= FFT_ST;
      	addr_temp_in <= start_addrs_reg;
      end if;
      if middle_delay_first_cycle='0' then --Remove this condition if you need adr_en=1 during the middle_delay
          addr_en        <= '1';
      else 
          addr_en        <= '0';
      end if;       
      if middle_delay_flag = '1' then
      	middle_delay_count_en <= '1';
      	addr_count_halt_en    <= '1';
      	step_val_temp_in      <= (OTHERS => '0');
      	add_sub_addr          <= (OTHERS => '0');
      end if;

      WHEN OTHERS => NULL;
      END CASE;
      END PROCESS AGU_FSM;


  State_Reg : PROCESS (clk, rst_n)
  BEGIN 
    IF rst_n = '0' THEN
      pres_state <= IDLE_ST;
    ELSIF clk'event AND clk = '1' THEN
      pres_state <= next_state;
    END IF;
  END PROCESS State_Reg;

END behave;
