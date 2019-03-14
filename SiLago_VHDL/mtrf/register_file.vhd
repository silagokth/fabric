-------------------------------------------------------
--! @file
--! @brief Register file
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
-- Title      : Register file
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File        : register_file.vhd
-- Author      : sadiq  <sadiq@drrasystem>
-- Company     : 
-- Created     : 2013-07-19
-- Last update : 2019-03-11
-- Platform    : 
-- Standard    : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2013
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author    Description
-- 2013-07-19  1.0      sadiq 	  Created
-- 2019-03-11  2.0      Dimitris  Fix some DiMArch bugs
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

--! IEEE Library
library ieee;
--! Use standard library
use ieee.std_logic_1164.all;
--! Use numeric library for signed and unsigned arithmetics
use ieee.numeric_std.all;
--! Use unsigned library, treat all std_vector as unsigned numbers
USE ieee.std_logic_unsigned.ALL; -- @TODO Need to remove the unsigned library, only the numeric_std library should be used
--! Use the top constant package that includes all the constants and type definitions
use work.top_consts_types_package.all;

--! @brief This is the register file of the DRRA cell
--! @detail The register file of the DRRA cell. It has 4 ports in total.
--! Two read and two write ports, namely port 0 and port 1.
--! The register is compossed by register rows, the register rows is then grouped to 
--! blocks equal to the size of one DiMArch row.
--! Port 0 is used to access one register file row [access from and to some DRRA cell].
--! Port 0 is also used to acces a register file block [access form and to some DiMArch cell].
--! Port 1 is used to access one register file row [access from and to some DRRA cell].
ENTITY register_file IS
	PORT(
		clk                              : IN  std_logic;
		rst_n                            : IN  std_logic;
		rd_tb                            : IN  std_logic;
		dimarch_mode                     : in  std_logic;
		rd_inst_start_0, wr_inst_start_0 : in  std_logic;
		wr_addr_0                        : IN  std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
		wr_addr_1                        : IN  std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
		wr_addr_tb                       : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
		wr_data_ready_0                  : IN  std_logic;
		wr_data_ready_1                  : IN  std_logic;
		wr_data_ready_tb                 : IN  std_logic;
		rd_addr_0                        : IN  std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
		rd_addr_1                        : IN  std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
		rd_addr_tb                       : IN  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
		rd_0                             : IN  std_logic;
		rd_1                             : IN  std_logic;
		data_in_0                        : IN  signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
		data_in_1                        : IN  signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
		data_in_2                        : IN  signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
		reg_outp_cntrl                   : IN  std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE - 1 DOWNTO 0);
		dimarch_data_in                  : in  STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
		dimarch_data_out                 : out STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
		dimarch_rd_2_out                 : out std_logic;
		data_out_reg_0_right             : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
		data_out_reg_0_left              : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
		data_out_reg_1_right             : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
		data_out_reg_1_left              : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
		data_out_2                       : OUT signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0)
	);
	
END register_file;



ARCHITECTURE struct OF register_file IS
  
  COMPONENT register_row 
    PORT (
      rst_n   : IN  std_logic;
      clk     : IN  std_logic;
      wr_enb  : IN  std_logic;
      reg_in  : IN  signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
      reg_out : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0));
  END COMPONENT;
  constant data_out_2_unassigned                       :  signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0) := (others => 'Z');
  ---------------------------------------------------------------------------------------------------
  -- dimarch control signals
  ---------------------------------------------------------------------------------------------------
  signal rd_2,wr_data_ready_2      : std_logic;
  signal wr_addr_2   :  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH-1 downto 0);
  signal rd_addr_2   :  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH-1 downto 0);
---------------------------------------------------------------------------------------------------
  TYPE   reg_file_type IS ARRAY (0 TO REG_FILE_DEPTH - 1) OF signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
  SIGNAL wr_enb_0                                    : std_logic_vector(REG_FILE_DEPTH - 1 DOWNTO 0);
  SIGNAL wr_enb_1                                    : std_logic_vector(REG_FILE_DEPTH - 1 DOWNTO 0);
  SIGNAL wr_enb                                : std_logic_vector(REG_FILE_DEPTH - 1 DOWNTO 0);
  SIGNAL wr_addr_enb_0, wr_addr_enb_1          : std_logic_vector(REG_FILE_DEPTH - 1 DOWNTO 0);
  SIGNAL wr_addr_enb_2, wr_enb_2, wr_enb_2_prev           : std_logic_vector(NR_REG_FILE_DATA_BLOCKS - 1 DOWNTO 0);
  SIGNAL reg_in, reg_out, temp_data                  : reg_file_type;
  TYPE dimarch_mode_type IS (rd,wr,both,none);

  signal dimarch_mode_current_state : dimarch_mode_type;
  signal dimarch_mode_current_state_final : dimarch_mode_type;
  signal 		 data_in_reg_2_tmp        : signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
  signal 		 data_out_2_tmp        : signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
  
--data_out_2
  --constant block_0   :  std_logic_vector(REG_FILE_MEM_DATA_WIDTH-1 downto 0):=(others => '0');

  
BEGIN  -- struct 
 -----------------------------------------------------------------------------------------------------
 -- DiMArch read/write connections
 -- DiMArch is always read from port 0
 -- if the address is even it will read/write 0th block
 -- if the address is odd it will  1st block
 -----------------------------------------------------------------------------------------------------
	rd_2  <=  rd_0 when (dimarch_mode_current_state_final = rd or dimarch_mode_current_state_final = both) else '0';
	wr_data_ready_2 <=wr_data_ready_0 when dimarch_mode_current_state_final = wr or dimarch_mode_current_state_final = both else wr_data_ready_tb; 
	wr_addr_2 <= wr_addr_0(REG_FILE_MEM_ADDR_WIDTH-1 downto 0)when dimarch_mode_current_state_final = wr or dimarch_mode_current_state_final = both else wr_addr_tb when (wr_data_ready_tb = '1' ) else (others => '0');
	rd_addr_2 <= rd_addr_0(REG_FILE_MEM_ADDR_WIDTH-1 downto 0)when dimarch_mode_current_state_final = rd or dimarch_mode_current_state_final = both else rd_addr_tb when (rd_tb = '1' ) else (others => '0');
	
	PROC_DIMARCH_MODE_FINAL: process(dimarch_mode, rd_inst_start_0, wr_inst_start_0, dimarch_mode_current_state)
	begin
		if rd_inst_start_0 = '1' then
			if dimarch_mode =  '1' then
				if dimarch_mode_current_state = none then
					dimarch_mode_current_state_final <= rd;
				elsif dimarch_mode_current_state = wr then
					dimarch_mode_current_state_final <= both;
				else
					dimarch_mode_current_state_final <= dimarch_mode_current_state;
				end if;
			else
				if dimarch_mode_current_state = rd then
					dimarch_mode_current_state_final <= none;
				elsif dimarch_mode_current_state = both then
					dimarch_mode_current_state_final <= wr;
				else
					dimarch_mode_current_state_final <= dimarch_mode_current_state;
				end if;
			end if;
		elsif wr_inst_start_0 = '1' then
			if dimarch_mode =  '1' then
				if dimarch_mode_current_state = none then
					dimarch_mode_current_state_final <= wr;
				elsif dimarch_mode_current_state = rd then
					dimarch_mode_current_state_final <= both;
				else
					dimarch_mode_current_state_final <= dimarch_mode_current_state;
				end if;
			else
				if dimarch_mode_current_state = wr then
					dimarch_mode_current_state_final <= none;
				elsif dimarch_mode_current_state = both then
					dimarch_mode_current_state_final <= rd;
				else
					dimarch_mode_current_state_final <= dimarch_mode_current_state;
				end if;
			end if;
		else
			dimarch_mode_current_state_final <= dimarch_mode_current_state;
		end if;
	end process;

-- 
 -----------------------------------------------------------------------------------------------------
 -- Store DiMArch Mode until next instruction 
 -- DiMArch is always read from port 0
 -- if the address is even it will read/write 0th block
 -- if the address is odd it will  1st block
 -----------------------------------------------------------------------------------------------------
 p_dimarch_mode_reg : process (clk, rst_n) is
 begin
 	if rst_n = '0' then
 		dimarch_mode_current_state <= none;
 	elsif rising_edge(clk) then
		wr_enb_2_prev <= wr_enb_2;
 		if rd_inst_start_0  = '1' or wr_inst_start_0 = '1' or wr_enb_2 = 0 then
 			case dimarch_mode_current_state is 
 				when rd =>
 					if rd_inst_start_0  = '1' and dimarch_mode = '0' then
 						dimarch_mode_current_state   <= none;
 					elsif wr_inst_start_0 = '1' and dimarch_mode = '1' then
 						dimarch_mode_current_state   <= both;
 					else
 						dimarch_mode_current_state   <= rd;
 					end if; 				
 				when wr =>
 					if rd_inst_start_0  = '1' and dimarch_mode = '1' then
 						dimarch_mode_current_state   <= both;
 					elsif (wr_inst_start_0 = '1' and dimarch_mode = '0') then
 						dimarch_mode_current_state   <= none;
 					else
 						dimarch_mode_current_state   <= wr;
 					end if;
 				when both =>
 					if rd_inst_start_0  = '1' and dimarch_mode = '0' then
 						dimarch_mode_current_state   <= wr;
 					elsif wr_inst_start_0 = '1' and dimarch_mode = '0' then
 						dimarch_mode_current_state   <= rd;
 					else
 						dimarch_mode_current_state   <= both;
 					end if;
 				when none =>
 					if rd_inst_start_0  = '1' and dimarch_mode = '1' then
 						dimarch_mode_current_state   <= rd;
 					elsif wr_inst_start_0 = '1' and dimarch_mode = '1' then
 						--assert false report "DIMARCH WRITE MODE ACTIVATED " severity error;
 						
 						dimarch_mode_current_state   <= wr;
 					else
 						dimarch_mode_current_state   <= none;
 					end if;
 			end case;
 		end if;
 	end if;
 end process p_dimarch_mode_reg;
 
 
  data_in_reg_2_tmp  <= data_in_2 when (dimarch_mode_current_state = none) else  signed(dimarch_data_in);
 
 
-- p_dimarch_data_out : process(dimarch_mode_current_state,data_out_2_tmp) is
-- begin
-- 	if dimarch_mode_current_state = rd or dimarch_mode_current_state = both then
-- 		dimarch_data_out  <= std_logic_vector(data_out_2_tmp);
-- 	else
-- 		dimarch_data_out  <= block_0;
-- 	end if;
-- end process p_dimarch_data_out;
 dimarch_data_out  <= std_logic_vector(data_out_2_tmp);
 dimarch_rd_2_out   <= rd_2;
 
 
 --dimarch_data_out  <= std_logic_vector(data_out_2_tmp) when (dimarch_mode_current_state = rd or dimarch_mode_current_state = both) else block_0;
 
 --data_out_2  <=  data_out_2_tmp when (dimarch_mode_current_state = none ) else signed(block_0);
 
  -----------------------------------------------------------------------------------------------------
  -- wr_addr binary to grey conversion  
 -----------------------------------------------------------------------------------------------------
 
  G_WR_ADDR_ENB_0 : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
  BEGIN
    wr_addr_enb_0(i) <= '1' WHEN wr_addr_0 = i ELSE '0';
  END GENERATE G_WR_ADDR_ENB_0;

  G_WR_ADDR_ENB_1 : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
  BEGIN
    wr_addr_enb_1(i) <= '1' WHEN wr_addr_1 = i ELSE '0';
  END GENERATE G_WR_ADDR_ENB_1;
  
  G_WR_ADDR_ENB_2 : FOR i IN 0 TO NR_REG_FILE_DATA_BLOCKS-1 GENERATE
  BEGIN
    wr_addr_enb_2(i) <= '1' WHEN wr_addr_2 = i ELSE '0';
  END GENERATE G_WR_ADDR_ENB_2;

 -----------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- BLOCK MODIFIED BY Yu Yang
--   Use generic scheme to generate enable signal for each row.
--   The enable signal for each row should be the resoult of OR of wr_enb_0,
--     wr_enb_1 and wr_enb_2. Since wr_enb_2 is for dimarch, it only has length
--     equal to the number of banks.
--
-- ORIGINAL CODE BEGIN ---------------------------------------------------------
--
---- these two block should be generated by one generate statement 
--  G4:for i in 0 to 15 generate
--  begin
--    wr_enb(i) <= (wr_enb_0(i) or wr_enb_1(i))  when (wr_enb_2 = "00") else wr_enb_2(0);
--  end generate G4;
--  
--  G5:for i in 16 to 31 generate
--  begin
--     wr_enb(i) <= (wr_enb_0(i) or wr_enb_1(i))  when (wr_enb_2 = "00") else wr_enb_2(1);
--  end generate G5;
--  
--  -- TODO: CHECK HERE: Register depth is fixed to 32???
----  G6:for i in 32 to 47 generate
----  begin
----     wr_enb(i) <= (wr_enb_0(i)) or wr_enb_1(i) or (wr_enb_2(2));
----  end generate G6;
----  
----  G7:for i in 48 to 63 generate
----  begin
----     wr_enb(i) <= (wr_enb_0(i)) or wr_enb_1(i) or (wr_enb_2(3));
----  end generate G7;
--
-- ORIGINAL CODE END -----------------------------------------------------------
--
-- MODIFIED CODE BEGIN ---------------------------------------------------------
--
	GE_WR_ENB : for i in 0 to REG_FILE_DEPTH-1 generate
		wr_enb(i) <= wr_enb_0(i) or wr_enb_1(i) or wr_enb_2(i/MEM_BLOCK_SIZE);
	end generate GE_WR_ENB;
--
-- MODIFIED CODE END -----------------------------------------------------------
--
--------------------------------------------------------------------------------

  G_WR_ENB_0 : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
  BEGIN
--------------------------------------------------------------------------------
-- MODIFICATION BEGIN: FIX DIMARCH MODE BUG
--------------------------------------------------------------------------------
-- ORIGINAL CODE ---------------------------------------------------------------
--  wr_enb_0(i) <= (wr_addr_enb_0(i) AND wr_data_ready_0 );
--------------------------------------------------------------------------------
-- MODIFIED CODE ---------------------------------------------------------------
    wr_enb_0(i) <= (wr_addr_enb_0(i) AND wr_data_ready_0 ) when (dimarch_mode_current_state_final = none or dimarch_mode_current_state_final = rd) else '0';
--------------------------------------------------------------------------------
-- MODIFICATION END
--------------------------------------------------------------------------------
  END GENERATE G_WR_ENB_0;

  G_WR_ENB_1 : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
  BEGIN
    wr_enb_1(i) <= wr_addr_enb_1(i) AND wr_data_ready_1;
  END GENERATE G_WR_ENB_1;
  
  G_WR_ENB_2 : FOR i IN 0 TO NR_REG_FILE_DATA_BLOCKS-1 GENERATE
  BEGIN
    wr_enb_2(i) <= wr_addr_enb_2(i) AND wr_data_ready_2;
  END GENERATE G_WR_ENB_2;

--  G_WR_ENB : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
--  BEGIN
--    wr_enb(i) <= wr_enb_0(i) OR wr_enb_1(i);
--  END GENERATE G_WR_ENB;

--  G_TEMP:for i in 0 to 63 generate
--  begin
--  reg_in(i) <= temp_data(i);
--  end generate G_TEMP;

  G_DATA_IN : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
  BEGIN
    temp_data(i) <= data_in_0 WHEN wr_enb_0(i) = '1' ELSE data_in_1 when wr_enb_1(i) = '1' ELSE (others=>'0'); --wr_enb_0 = '0' and wr_enb_1 = '0' okay to make 0 to avoid latch??
  END GENERATE G_DATA_IN;

  G_REGFILE : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
  BEGIN
    register_array : register_row
      PORT MAP (
        rst_n   => rst_n,
        clk     => clk,
        wr_enb  => wr_enb(i),
        reg_in  => reg_in(i),
        reg_out => reg_out(i));
  END GENERATE G_REGFILE;

                                                       
 -- GEN_BITWIDTH_REG_IN: For I in 0 to 1 generate
    GEN_32B_REG_IN: if BITWIDTH=32 generate
      
      process(data_in_reg_2_tmp, wr_enb_2, temp_data)
      
        begin
        IF( wr_enb_2 /= 0) then

          reg_in <= (others => ((others => '0')));

          for i in 0 to 7 loop
            reg_in(i)      <= data_in_reg_2_tmp((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
            reg_in(i + 16) <= data_in_reg_2_tmp((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
          end loop;
          --for i in 8 to 15 loop
          --  reg_in(i)      <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
          --  reg_in(i + 16) <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
          --end loop;

 --/////comment back for registerfile with depth 64:start
--          for i in 0 to 7 loop
--            reg_in(i + 32) <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--            reg_in(i + 48) <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--          end loop;
--          --for i in 8 to 15 loop
--          --  reg_in(i + 32) <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--          --  reg_in(i + 48) <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--          --end loop;
--/////comment back for registerfile with depth 64:end 
        else 
          reg_in <= (others => ((others => '0')));

          for i in 0 to 7 loop
            reg_in(i)      <= temp_data(i);
            reg_in(i + 16) <= temp_data(i + 16);
            --reg_in(i + 8)  <= temp_data(i + 8);
            --reg_in(i + 24) <= temp_data(i + 24);
          end loop;

--/////comment back for registerfile with depth 64:start
--          for i in 32 to 39 loop
--            reg_in(i)      <= temp_data(i);
--            reg_in(i + 16) <= temp_data(i + 16);
--            --reg_in(i + 8)  <= temp_data(i + 8);
--            --reg_in(i + 24) <= temp_data(i + 24);
--          end loop;
--/////comment back for registerfile with depth 64:end
        end if; 
      end process;
    end generate GEN_32B_REG_IN;






--------------------------------------------------------------------------------
-- BLOCK MODIFIED BY Yu Yang
--   Use generic scheme to assign the reg_in for each row.
--
-- ORIGINAL CODE BEGIN ---------------------------------------------------------
--
--    GEN_8_16_B_REG_IN: if BITWIDTH=8 or BITWIDTH=16 generate
--      process(data_in_reg_2_tmp, wr_enb_2, temp_data)
--      begin
--        if (wr_enb_2 /= 0) then

--          reg_in <= (others => ((others => '0')));

--          for i in 0 to 15 loop
--            reg_in(i)      <= data_in_reg_2_tmp((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--            reg_in(i + 16) <= data_in_reg_2_tmp((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--          -- /////comment back for registerfile with depth 64
--          -- reg_in(i + 32) <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--          -- reg_in(i + 48) <= data_in_2((i+1) * BITWIDTH - 1 downto i * BITWIDTH);
--          end loop;
--	else
--		         reg_in <= (others => ((others => '0')));

--          --/////comment back for registerfile with depth 64
--          --for i in 0 to 63 loop
--          for i in 0 to 31 loop
--            reg_in(i) <= temp_data(i);
--          end loop;
--        end if; 
--      end process;
--    end generate GEN_8_16_B_REG_IN;
--    
--   -- end generate GEN_BITWIDTH_REG_IN;
--
-- ORIGINAL CODE END -----------------------------------------------------------
--
-- MODIFIED CODE BEGIN ---------------------------------------------------------
--
	GEN_8_16_B_REG_IN: if BITWIDTH=8 or BITWIDTH=16 generate
		process(data_in_reg_2_tmp, wr_enb_2, temp_data)
		begin
			if (wr_enb_2 /= 0) then
				reg_in <= (others => ((others => '0')));
				for i in 0 to NR_REG_FILE_DATA_BLOCKS-1 loop
					for j in 0 to WORDS_PER_BLOCK-1 loop
						reg_in(i*MEM_BLOCK_SIZE+j)      <= data_in_reg_2_tmp((j+1) * BITWIDTH - 1 downto j * BITWIDTH);
					end loop;
				end loop;
			else
				reg_in <= (others => ((others => '0')));
				for i in 0 to REG_FILE_DEPTH-1 loop
					reg_in(i) <= temp_data(i);
				end loop;
			end if; 
		end process;
	end generate GEN_8_16_B_REG_IN;
--
-- MODIFIED CODE END -----------------------------------------------------------
--
--------------------------------------------------------------------------------

  --data_out_0 <= reg_out(to_integer(unsigned(rd_addr_0)));-- when reg_outp_cntrl(0) = '1' else (OTHERS => '0');
  --data_out_1 <= reg_out(to_integer(unsigned(rd_addr_1)));-- when reg_outp_cntrl(1) = '1' else (OTHERS => '0');
  data_out_reg_0_right  <= reg_out(to_integer(unsigned(rd_addr_0))) when (reg_outp_cntrl(0)='1' and rd_0='1') else (others =>'0');
  data_out_reg_1_right  <= reg_out(to_integer(unsigned(rd_addr_1))) when (reg_outp_cntrl(0)='1' and rd_1='1') else (others =>'0');
  data_out_reg_0_left   <= reg_out(to_integer(unsigned(rd_addr_0))) when (reg_outp_cntrl(1)='1' and rd_0='1') else (others =>'0');
  data_out_reg_1_left   <= reg_out(to_integer(unsigned(rd_addr_1))) when (reg_outp_cntrl(1)='1' and rd_1='1') else (others =>'0');

    
   -- GEN_BITWIDTH: For I in 0 to 1 generate
--      GEN_32B_CASE: if BITWIDTH=32 generate
--      
--      P_data_out_2_tmp: process(rd_addr_2, reg_out, rd_2)
--      	variable rd_addr_2_int : integer range 0 to 2**REG_FILE_MEM_ADDR_WIDTH;
--      begin
--      	rd_addr_2_int :=to_integer(unsigned(rd_addr_2));
--        if (rd_2 = '1') then
--          case rd_addr_2_int is
--            when 0 =>
--              data_out_2_tmp(REG_FILE_MEM_DATA_WIDTH-1 downto 0) <= (others=> '0'); 
--              for i in 0 to 7 loop
--                data_out_2_tmp((i+1) * BITWIDTH-1 downto i * BITWIDTH) <= reg_out(i);
--              end loop;
--
--            when others =>--"01"
--              data_out_2_tmp(REG_FILE_MEM_DATA_WIDTH-1 downto 0) <= (others=> '0');
--              for i in 0 to 7 loop
--                data_out_2_tmp((i+1) * BITWIDTH-1 downto i * BITWIDTH) <= reg_out(i + 16);
--              end loop;

--/////comment back for registerfile with depth 64:start
--            when "10" =>
--              data_out_2(REG_FILE_MEM_DATA_WIDTH-1 downto 0) <= (others=> '0');
--              for i in 0 to 7 loop
--                data_out_2((i+1) * BITWIDTH-1 downto i * BITWIDTH) <= reg_out(i + 32);
--              end loop;

--            when others =>
--              data_out_2(REG_FILE_MEM_DATA_WIDTH-1 downto 0) <= (others=> '0');
--              for i in 0 to 7 loop
--                data_out_2((i+1) * BITWIDTH-1 downto i * BITWIDTH) <= reg_out(i + 48);
--              end loop;
--/////comment back for registerfile with depth 64:end
--          end case;
--        else 
--          data_out_2_tmp <= (others => '0');
--        end if;
--      end process;
--      
--    end generate GEN_32B_CASE;
    
    --GEN_8_16_B_CASE : if BITWIDTH = 8 or BITWIDTH = 16 generate
    	process(rd_addr_2, reg_out, rd_2,data_out_2_tmp)
    		variable rd_addr_2_int : integer range 0 to (2 ** REG_FILE_MEM_ADDR_WIDTH)-1;
    	begin
    		rd_addr_2_int := to_integer(unsigned(rd_addr_2));
    		if (rd_2 = '1') then
    			case rd_addr_2_int is
    				when 0 =>
    					-- assert false report "read regout block 0" severity note;
    					data_out_2_tmp(REG_FILE_MEM_DATA_WIDTH - 1 downto 0) <= (others => '0');
    					for i in 0 to WORDS_PER_BLOCK-1 loop
    						data_out_2_tmp((i + 1) * BITWIDTH - 1 downto i * BITWIDTH) <= reg_out(i);
    					end loop;
    				when 1 =>      --"01" =>
    					data_out_2_tmp(REG_FILE_MEM_DATA_WIDTH - 1 downto 0) <= (others => '0');
    					for i in 0 to WORDS_PER_BLOCK-1 loop
    						data_out_2_tmp((i + 1) * BITWIDTH - 1 downto i * BITWIDTH) <= reg_out(i + 16);
    					end loop;
    			--/////comment back for registerfile with depth 64:start
    			        when 2 =>
    			          data_out_2_tmp(REG_FILE_MEM_DATA_WIDTH-1 downto 0) <= (others=> '0');
    			          for i in 0 to 15 loop
    			            data_out_2_tmp((i+1) * BITWIDTH-1 downto i * BITWIDTH) <= reg_out(i + 32);
    			          end loop;
    			        when others =>
    			          data_out_2_tmp(REG_FILE_MEM_DATA_WIDTH-1 downto 0) <= (others=> '0');
    			          for i in 0 to 15 loop
    			            data_out_2_tmp((i+1) * BITWIDTH-1 downto i * BITWIDTH) <= reg_out(i + 48);
    			          end loop;
    			--/////comment back for registerfile with depth 64:end
    			end case;
    		else
    			data_out_2_tmp <= (others => '0');
    		end if;
  	
  	end process;
  		data_out_2  <= data_out_2_tmp when (rd_tb = '1') else data_out_2_unassigned;
  	
--    end generate GEN_8_16_B_CASE;
  
  --assert data_out_2_tmp = (others ( => '0')) report "dataout" severity error;
  

--  end generate GEN_BITWIDTH;

END struct;
  

