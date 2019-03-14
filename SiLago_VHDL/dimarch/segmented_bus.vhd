---------------- Copyright (c) notice -----------------------------------------
--
-- The VHDL code, the logic and concepts described in this file constitute
-- the intellectual property of the authors listed below, who are affiliated
-- to KTH(Kungliga Tekniska Hogskolan), School of ICT, Kista.
-- Any unauthorised use, copy or distribution is strictly prohibited.
-- Any authorised use, copy or distribution should carry this copyright notice
-- unaltered.
-- This is a segment of bus that implements without  
-- instruction value 2 means right to left
-- instruction value 1 means left to right
-- instruction value 0 means break
-- instruction value 3 not used
--- Authors: Muhammad Adeel Tajammul: PhD student, ES, School of ICT, KTH, Kista.
-- Contact: tajammul@kth.se
-----------------------------------------------------------------------------------
--   DIAGRAM
-----------------------------------------------------------------------------------
--
--
--
--
--
--
--
--                                                                          
-- PARTITION_INST_left(i,j)  ______________________________________________   PARTITION_INST_right(i+1,j)   ___________________________________ PARTITION_INST_left(i+2,j)
--               ----------->|left inst                          right inst |<---------------------------- |left_inst_out        right_inst_out| ----------->left inst
--                           |         Bus segment                          |                              |                                   | SRAM_INST_hor_left_in(i,1)
--SRAM_INST_hor_left_in(i,j) |                                              | SRAM_INST_hor_right_in(i+1,j)|hor_bus_left_out  hor_bus_right_out| ----------->left in
--               ----------->|left in    SEGMENT_BUS               right in |<---------------------------- |           STILE                   |
--                           |         Bus segment                          |                              |                                   |
--SRAM_INST_hor_left_out(i,j)|                                              |SRAM_INST_hor_right_out(i+1,j)|hor_bus_left_in    hor_bus_right_in|SRAM_INST_hor_left_out(i+1,j)
--              <------------|left out                            right out |----------------------------> |___________________________________| <------------left out
--                           |______________________________________________|                                               ^ east_splitter_direction
--                                             |                                                                            |
--                                             |                                                                            |
--                                             |                                                                            |
--                                             |status                                                                      |
--           __________________________________|____________________________________________________________________________|

-- PARTITION_INST_right(i,j)     ___________________________________    PARTITION_INST_left(i,j)       ______________________________________________   PARTITION_INST_right(i+1,j)   ___________________________________ PARTITION_INST_left(i+1,j)   
--<---------------------------- |left_inst_out        right_inst_out| ------------------------------->|left inst                          right inst |<---------------------------- |left_inst_out        right_inst_out| ----------->left inst       
--                              |                                   |                                 |         Bus segment                          |                              |                                   | SRAM_INST_hor_left_in(i+1,1)  
-- SRAM_INST_hor_right_in(i,j)  |hor_bus_left_out  hor_bus_right_out|  SRAM_INST_hor_left_in(i,j)     |                                              | SRAM_INST_hor_right_in(i+1,j)|hor_bus_left_out  hor_bus_right_out| ----------->left in         
--<---------------------------- |           STILE                   |-------------------------------->|left in    SEGMENT_BUS               right in |<---------------------------- |           STILE                   |                             
--                              |                                   |                                 |         Bus segment                          |                              |                                   |                             
--SRAM_INST_hor_right_out(i,j)|hor_bus_left_in      hor_bus_right_in|  SRAM_INST_hor_left_out(i,j)    |                                              |SRAM_INST_hor_right_out(i+1,j)|hor_bus_left_in    hor_bus_right_in|SRAM_INST_hor_left_out(i+1,j)
------------------------------> |___________________________________| <-------------------------------|left out                            right out |----------------------------> |___________________________________| <------------left out       
--                                               ^ west_splitter_direction                            |______________________________________________|                                               ^ east_splitter_direction                        
--                                               |                                                                                             |                                                                            |                                                
--                                               |                                                                                             |                                                                            |                                                
--                                               |                                                                                             |                                                                            |                                                
--                                               |                                                                                             |status                                                                      |                                                
--|                                               _____________________________________________________________________________________________|____________________________________________________________________________|                                                
--
--
--
--  ___________________________________    PARTITION_INST_top(i,0)       ______________________________________________   PARTITION_INST_bottom(i,j-1)   ___________________________________ PARTITION_INST_left(i+1,j)   
-- |                top_instruction_out| ------------------------------->|left inst                          right inst |<---------------------------- |bottom_inst_out        top_inst_out| ----------->left inst       
-- |                                   |                                 |         Bus segment                          |                              |                                   | SRAM_INST_ver_left_in(i+1,1)  
-- |                    ver_bus_top_out|  SRAM_INST_ver_left_in(i,0)     |                                              | SRAM_INST_ver_right_in(i,j-1)|ver_bus_bottom_out  ver_bus_top_out| ----------->left in         
-- |           contile                 |-------------------------------->|left in    SEGMENT_BUS               right in |<---------------------------- |           STILE                   |                             
-- |                                   |                                 |         Bus segment                          |                              |                                   |                             
-- |                     ver_bus_top_in|  SRAM_INST_ver_left_out(i,0)    |                                              |SRAM_INST_ver_right_out(i,j-1)|ver_bus_bottom_in    ver_bus_top_in|SRAM_INST_ver_left_out(i+1,j)
-- |___________________________________| <-------------------------------|left out                            right out |----------------------------> |___________________________________| <------------left out       
--              ^ north_splitter_direction                               |______________________________________________|                                               ^ east_splitter_direction                        
--              |                                                                                             |                                                                            |                                                
--              |                                                                                             |                                                                            |                                                
--              |                                                                                             |                                                                            |                                                
--              |                                                                     bus_direction_ver       |status                                                                      |                                                
--              |_____________________________________________________________________________________________|____________________________________________________________________________|                                                
--
--
--
--
--
--
---------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.noc_types_n_constants.all;
entity segmented_bus is
	port(
		clk               : in  std_logic;
		rst               : in  std_logic;
		left_instruction  : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		right_instruction : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		left_in           : in  NOC_BUS_TYPE;
		right_in          : in  NOC_BUS_TYPE;
		left_out          : out NOC_BUS_TYPE;
		right_out         : out NOC_BUS_TYPE;
		bus_direction     : out std_logic_vector(1 DOWNTO 0)
	);
end entity segmented_bus;

architecture RTL of segmented_bus is
	signal direction : std_logic_vector(1 DOWNTO 0);
begin
	p_segment : process(clk, rst) is
	begin
		if rst = '0' then
			--			left_out  <= IDLE_BUS;
			--			right_out  <= IDLE_BUS;
			direction <= (others => '0');
		elsif rising_edge(clk) then
			if left_instruction.ENABLE = '1' then
				if left_instruction.PARTITION = "10" then
					direction <= left_instruction.PARTITION;
				elsif left_instruction.PARTITION = "01" then
					direction <= left_instruction.PARTITION;
				else                    -- invalid instruction
					assert false report "invalid partitioning" severity error;
				end if;
			elsif right_instruction.ENABLE = '1' then
				if right_instruction.PARTITION = "10" then
					direction <= right_instruction.PARTITION;
				elsif right_instruction.PARTITION = "01" then
					direction <= right_instruction.PARTITION;
				else                    -- invalid instruction
					assert false report "invalid partitioning" severity error;
				end if;
			end if;
		end if;
	end process p_segment;

	p_direction : process(left_in, right_in, direction) is
	begin
		case direction is
			when "10" =>
				left_out  <= IDLE_BUS;
				right_out <= left_in;
			when "01" =>
				left_out  <= right_in;
				right_out <= IDLE_BUS;
			when others =>
				left_out  <= IDLE_BUS;
				right_out <= IDLE_BUS;
		end case;

	end process p_direction;

	bus_direction <= direction;

end architecture RTL;
