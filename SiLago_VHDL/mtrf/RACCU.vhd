-------------------------------------------------------
--! @file
--! @brief raccu file
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
-- Title      : RACCU
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : RACCU.vhd
-- Author     : Nasim Farahini
-- Company    : KTH
-- Created    : 2014-04-15
-- Platform   : SiLago 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author          Description
-- 2014-04-15  1.0      Nasim Farahini
-- 2019-03-11  2.9      Dimitrios       Fix the signed and unsigned bug, see TODO
-------------------------------------------------------------------------------
-- @TODO: Only use numeric_std, and change all CONV_INTEGER functions

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

--! IEEE Library and the WORK Library
library ieee, work;
--! Use standard library
use ieee.std_logic_1164.all;
--! Use numeric library for signed and unsigned arithmetics
use ieee.numeric_std.all;
--! Use signed library, treat all std_vector as signed numbers
USE ieee.std_logic_signed.ALL; -- @TODO Need to remove the signed library, only the numeric_std library should be used
--! Use the top constant package that includes all the constants and type definitions
use work.top_consts_types_package.all;


--! @brief This is the RACCU
--! @detail The raccu is used to help with the loop managment.
--! It is an integer DPU with modes written in DPU_pkg.vhd file
ENTITY RACCU IS
  PORT (
    clk               : IN std_logic;
    rst_n             : IN std_logic;
    raccu_in1_sd      : IN std_logic;
    raccu_in1         : IN std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE-1 DOWNTO 0);  --signed
    raccu_in2_sd      : IN std_logic;
    raccu_in2         : IN std_logic_vector (RACCU_OPERAND2_VECTOR_SIZE-1 DOWNTO 0);  --signed
    raccu_cfg_mode    : IN std_logic_vector (RACCU_MODE_SEL_VECTOR_SIZE-1 DOWNTO 0);  --signed
    raccu_res_address : IN std_logic_vector (RACCU_RESULT_ADDR_VECTOR_SIZE-1 DOWNTO 0);  --signed
    raccu_regout      : OUT raccu_reg_out_ty;
    raccu_loop_reg    : OUT raccu_loop_array_ty
    );
END RACCU;

ARCHITECTURE beh OF RACCU IS
  SIGNAL loop_reg,loop_reg_tmp  : raccu_loop_array_ty;
  SIGNAL data_reg  : raccu_reg_out_ty;
  SIGNAL add_res, add_in1, add_in2, sub_in1, sub_in2, sub_res,no_of_iterations: std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE-1 DOWNTO 0);
  SIGNAL wr_enb,l_index_val_wr_enb ,l_end_flag_wr_enb, l_counter_wr_enb : std_logic;
  SIGNAL result : std_logic_vector (RACCU_REG_BITWIDTH-1 DOWNTO 0);
	signal debug_sig : unsigned(RACCU_MODE_SEL_VECTOR_SIZE-1 DOWNTO 0);
BEGIN
    raccu_regout <= data_reg;
    raccu_loop_reg <= loop_reg;
    add_res <= add_in1 + add_in2;
    sub_res <= sub_in1 - sub_in2;
	raccu_regout_process: process(rst_n,clk)
	begin
		if rst_n = '0' then
			data_reg <= (others=>(others=>'0'));
		elsif clk'event and clk = '1' then
			if wr_enb = '1' then
				data_reg(CONV_INTEGER(raccu_res_address)) <= result;
			end if;
		end if;
	end process raccu_regout_process;

	loop_reg_process: process(rst_n,clk)
	begin
		if rst_n = '0' then
			loop_reg <= (others => (loop_index_value => (OTHERS => '0'),loop_counter => (OTHERS => '0'),loop_end_flag=> '0'));
		elsif clk'event and clk = '1' then
			if l_index_val_wr_enb = '1' then
				loop_reg(CONV_INTEGER(raccu_res_address)).loop_index_value <= loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_index_value;
			end if;
			if l_counter_wr_enb = '1' then 
				loop_reg(CONV_INTEGER(raccu_res_address)).loop_counter <= loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_counter;
			end if;
			if l_end_flag_wr_enb='1' then 
				loop_reg(CONV_INTEGER(raccu_res_address)).loop_end_flag <= loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_end_flag;
			end if;
		end if;
	end process loop_reg_process;
	
  raccu_mode_process : PROCESS (raccu_cfg_mode,raccu_res_address,data_reg,loop_reg,raccu_in2_sd, raccu_in1_sd, raccu_in1, raccu_in2, sub_res, add_res, debug_sig)
    variable raccu_in1_tmp: std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE-1 DOWNTO 0):=(others=>'0');
    variable raccu_in2_tmp: std_logic_vector (RACCU_OPERAND1_VECTOR_SIZE-1 DOWNTO 0):=(others=>'0');
    variable debug_var : integer;
  BEGIN  -- process raccu_mode_process
    
    for i in 0 to MAX_NO_OF_RACCU_LOOPS-1 loop
      loop_reg_tmp(i) <= (loop_index_value => loop_reg(i).loop_index_value,loop_counter => loop_reg(i).loop_counter,loop_end_flag=> loop_reg(i).loop_end_flag);
    end loop;

    l_index_val_wr_enb <='0';
    l_end_flag_wr_enb <='0';
    l_counter_wr_enb <='0';
    no_of_iterations <=(others=>'0');
    add_in1 <=(others=>'0');
    add_in2 <=(others=>'0');
    wr_enb  <='0';
    result <=(others=>'0');
    sub_in1 <=(others=>'0');
    sub_in2 <=(others=>'0');
    --raccu_in1_tmp <=(others=>'0');
    --raccu_in2_tmp <=(others=>'0');
    debug_var := to_integer(unsigned(raccu_cfg_mode));
    debug_sig <= unsigned(raccu_cfg_mode);
    CASE debug_var IS
    
      WHEN RAC_MODE_LOOP_HEADER =>  

        add_in1 <= loop_reg(CONV_INTEGER(raccu_res_address)).loop_counter ;
        add_in2 <= "0000001";

        if loop_reg(CONV_INTEGER(raccu_res_address)).loop_counter = 0 then
          loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_index_value <= raccu_in1;--loading start index value
          l_index_val_wr_enb <='1';
        end if;

        if raccu_in2_sd='0' then 
          no_of_iterations <= raccu_in2;
        else 
          no_of_iterations <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
        end if ;

        if raccu_in2= add_res then --if no_of_iterations= add_res then , bug of delta delay
          l_end_flag_wr_enb <='1';
          loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_end_flag <= '1';
        else 
          l_counter_wr_enb <= '1';
          loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_counter <= add_res;
        end if;

      WHEN RAC_MODE_LOOP_TAIL =>   
 
			if loop_reg(CONV_INTEGER(raccu_res_address)).loop_end_flag='0' then    		                  
				add_in1 <= loop_reg(CONV_INTEGER(raccu_res_address)).loop_index_value ;
				add_in2 <= raccu_in1;
				loop_reg_tmp(CONV_INTEGER(raccu_res_address)).loop_index_value <= add_res;
				l_index_val_wr_enb <='1';	
			else 
				l_end_flag_wr_enb <='1';
				l_index_val_wr_enb <='1';
				l_counter_wr_enb <='1';
				loop_reg_tmp <= (others => (loop_index_value => (OTHERS => '0'),loop_counter => (OTHERS => '0'),loop_end_flag=> '0'));		
			end if; 
			
                                          
      WHEN RAC_MODE_ADD =>  
    		if raccu_in1_sd='0' then 
    			add_in1 <= raccu_in1;
    		else 
    			add_in1 <= data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
    		end if; 
    		if raccu_in2_sd='0' then 
    			add_in2 <= raccu_in2;
    		else 
    			add_in2 <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
    		end if;  
    		result  <= add_res;
    		wr_enb  <= '1';
      WHEN RAC_MODE_SUB =>    
     		if raccu_in1_sd='0' then 
    			sub_in1 <= raccu_in1;
    		else 
    			sub_in1 <= data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
    		end if; 
    		if raccu_in2_sd='0' then 
    			sub_in2 <= raccu_in2;
    		else 
    			sub_in2 <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
    		end if;  
    		result  <= sub_res;
    		wr_enb  <= '1';     
      WHEN RAC_MODE_SHFT_R =>  
      
        if raccu_in1_sd='0' then 
          raccu_in1_tmp := raccu_in1;
        else 
          raccu_in1_tmp := data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
        end if; 
        if raccu_in2_sd='0' then 
          raccu_in2_tmp := raccu_in2;
        else 
          raccu_in2_tmp := data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
        end if;      

        result  <= std_logic_vector(unsigned(raccu_in1_tmp) srl CONV_INTEGER(raccu_in2_tmp)); 
        wr_enb  <= '1'; 
  			 
      WHEN RAC_MODE_SHFT_L =>  
      
        if raccu_in1_sd='0' then 
          raccu_in1_tmp := raccu_in1;
        else 
          raccu_in1_tmp := data_reg(CONV_INTEGER(raccu_in1(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
        end if; 
        if raccu_in2_sd='0' then 
          raccu_in2_tmp := raccu_in2;
        else 
          raccu_in2_tmp := data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
        end if; 

        result  <= std_logic_vector(unsigned(raccu_in1_tmp) sll CONV_INTEGER(raccu_in2_tmp));
        wr_enb  <= '1'; 

      -- TODO: check if the loop_counter contains the correct value from start_val to the end

      -- In this mode, op1 determines the loop address and RACCU should add op2 value (whether static or dynamic)
      -- to the current index value (loop counter) of the specified loop and write the result to the res_address
      WHEN RAC_MODE_ADD_WITH_LOOP_INDEX =>		
        ------------------------------------------------------------------------
        -- MODIFICATION: CHANGE THE RANGE FOR raccu_in_1 TO LOOP_REG_WIDTH TO
        --               AVOID RANGE MISMATCH IN TRANSITION OF STATES.
        ------------------------------------------------------------------------
        -- ORIGINAL CODE:
        -- add_in1 <= loop_reg(to_integer(unsigned(raccu_in1(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)))).loop_index_value;
        ------------------------------------------------------------------------
        -- MODIFIED CODE:
        add_in1 <= loop_reg(to_integer(unsigned(raccu_in1(LOOP_REG_WIDTH-1 DOWNTO 0)))).loop_index_value;
        ------------------------------------------------------------------------
        -- MODIFICATION END
        ------------------------------------------------------------------------
        if raccu_in2_sd='0' then 
          add_in2 <= raccu_in2;
        else 
          add_in2 <= data_reg(CONV_INTEGER(raccu_in2(RACCU_REG_ADDRS_WIDTH-1 DOWNTO 0)));
        end if;

        result  <= add_res;
        wr_enb  <= '1';
      WHEN OTHERS =>

    END CASE;
  END PROCESS raccu_mode_process;



END beh;
