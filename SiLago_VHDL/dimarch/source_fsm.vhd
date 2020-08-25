-------------------------------------------------------
--! @file source_fsm.vhd
--! @brief 
--! @details 
--! @author Muhammad Adeel Tajammul
--! @version 1.0
--! @date 
--! @bug NONE
--! @todo NONE
--! @copyright  GNU Public License [GPL-3.0].
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
-- Title      : 
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : source_fsm.vhd
-- Author     : Muhammad Adeel Tajammul
-- Company    : KTH
-- Created    : 
-- Last update: 
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2014
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
--             1.0      Muhammad Adeel Tajammul      Created
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU          #
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

-- This is a SRAM Tile which will be used to extend DRRA Fabric for memory communication
-- be changed by changing the generics
--
--- Authors: Muhammad Adeel Tajammul: PhD student, ES, School of ICT, KTH, Kista.
-- Contact: tajammul@kth.se
---------------------------------------------------------------------------------


library IEEE;
use IEEE.std_logic_1164.all;
use ieee.NUMERIC_STD.all;
--use work.drra_types_n_constants.all;
--use work.top_consts_types_package.SRAM_AGU_INSTR_WIDTH;
use work.top_consts_types_package.all;
use work.noc_types_n_constants.all;
--use work.SINGLEPORT_SRAM_AGU_types_n_constants.AGU_INSTR_WIDTH;
--use work.SINGLEPORT_SRAM_AGU_types_n_constants.outputcontrol_s;
--use work.SINGLEPORT_SRAM_AGU_types_n_constants.outputcontrol_WIDTH;
--use work.SINGLEPORT_SRAM_AGU_types_n_constants.outputcontrol_e;

--DECLARE PKGS 
entity source_fsm is
	generic(vertical : std_logic := '0');

	port(
		rst_n                    : in  std_logic;
		clk                      : in  std_logic;
		bus_in                   : in  NOC_BUS_TYPE;
		----------------------------
		--	Source Decoder Flags 
		----------------------------

		RETRANSMIT               : in  STD_LOGIC; -- is the data turning or not 
		SEGMENT_SRC              : in  STD_LOGIC; -- is this the source data 		(for routing)		
		SEGMENT_DST              : in  STD_LOGIC; -- is this the destination data (for routing)
		flip_transmit            : in  STD_LOGIC; -- is this an intermediate down and we need to flip the segment bit (for routing)
		--		rw                       : in  STD_LOGIC;
		--------------------------------------------------------
		--	Source Decoder PROPOSED PARTITION VALUES 
		--------------------------------------------------------
		NORTH_instruction        : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		SOUTH_instruction        : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		EAST_instruction         : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		WEST_instruction         : in  PARTITION_INSTRUCTION_RECORD_TYPE;
		----------------------------
		--	partition_status  from partition handler 
		----------------------------
		--		east_priority            : in  std_logic;
		east_splitter_direction  : in  std_logic_vector(1 downto 0);
		--		west_priority            : in  std_logic;
		west_splitter_direction  : in  std_logic_vector(1 downto 0);
		--		north_priority           : in  std_logic;
		north_splitter_direction : in  std_logic_vector(1 downto 0);
		--		south_priority           : in  std_logic;
		south_splitter_direction : in  std_logic_vector(1 downto 0);
		----------------------------
		--	Segmented bus output  
		----------------------------
		bus_out                  : out NOC_BUS_TYPE;
		bus_ver_sel              : out STD_LOGIC;
		bus_hor_sel              : out STD_LOGIC;
		turn_right               : out std_logic;
		turn_left                : out std_logic;
		----------------------------
		--	AGUs and handles input  
		----------------------------
		agu_en_r                 : out std_logic;
		agu_en_w                 : out std_logic;
		SRAM_AGU_instruction_r   : out sram_agu_instruction_type;
		SRAM_AGU_instruction_w   : out sram_agu_instruction_type
	);
end entity source_fsm;

architecture RTL of source_fsm is
	constant interm           : INTEGER := 3;
	--SIGNAL    PREV_BUS_IN              :   NOC_BUS_TYPE;
	alias i_ON                : std_logic is bus_in.INSTRUCTION(ON_l); -- ORIGIN NODE 1=DESTINATION
	alias i_outputcontrol     : std_logic is bus_in.INSTRUCTION(NoC_Bus_instr_width -sr_rw);

	type switch_state_type is (IDLE, WAIT4AGU);
	signal current_state, next_state : switch_state_type;

	signal corner_flag : std_logic;

	signal src_agu_flag : std_logic;
	signal dst_agu_flag : std_logic;
	
	
--	constant zero_halts : std_logic_vector(DEPRICATED_BITS_WIDTH-1 downto 0) := (others => '0');
--	signal turn_right, turn_left : std_logic; 

begin
	p_source_fsm : process(clk, rst_n) is
--		variable 		SRAM_AGU_instruction_r_cnct   :  std_logic_vector(SRAM_AGU_INSTR_WIDTH - 1 downto 0);
--		variable	    SRAM_AGU_instruction_w_cnct   :  std_logic_vector(SRAM_AGU_INSTR_WIDTH - 1 downto 0);
		
	begin
		if rst_n = '0' then
			SRAM_AGU_instruction_r <= sram_instr_zero;
			SRAM_AGU_instruction_w <= sram_instr_zero;
			next_state             <= IDLE;
			agu_en_r               <= '0';
			agu_en_w               <= '0';
			bus_ver_sel            <= '0';
			bus_hor_sel            <= '0';
			bus_out                <= IDLE_BUS;
			corner_flag            <= '0';
			turn_right             <= '0';
			turn_left              <= '0';

			src_agu_flag <= '0';
			dst_agu_flag <= '0';
		elsif rising_edge(clk) then
			SRAM_AGU_instruction_r <= sram_instr_zero;
			SRAM_AGU_instruction_w <= sram_instr_zero;
			bus_out                <= IDLE_BUS;
			agu_en_r               <= '0';
			agu_en_w               <= '0';
			bus_ver_sel            <= '0';
			bus_hor_sel            <= '0';
			next_state             <= IDLE;
			case current_state is
			when IDLE =>
---------------------------------------------------------------------------------------------------------------------------				
-- IDLE
---------------------------------------------------------------------------------------------------------------------------
					src_agu_flag <= '0';
					dst_agu_flag <= '0';
					corner_flag  <= '0';
					turn_right   <= '0';
					turn_left    <= '0';
					if bus_in.bus_enable = '1' then
						if bus_in.instr_code = both_instruction then -- ROUTING
							src_agu_flag <= '0';
							dst_agu_flag <= '0';
							----------------------------DESTINATION-----------------------------------------------------------------------------------------------  							
							if SEGMENT_DST = '1' then    -- if destination
								if RETRANSMIT = '1' then -- if corner node and destination node
									corner_flag <= '1';
									bus_out     <= bus_in;
									if vertical = '0' then -- define vertical or horizontal
										bus_ver_sel <= '1';
									else
										bus_hor_sel <= '1';
									end if;
								elsif flip_transmit = '1' then -- if it is intermediate node
									bus_out                     <= bus_in;
									bus_out.INSTRUCTION(interm) <= not bus_in.INSTRUCTION(interm);
								else    -- if it is just a normal destination node  then just wait for agu instruction																		
									dst_agu_flag <= '1';
									next_state   <= WAIT4AGU;
								end if;
							----------------------------SOURCE-----------------------------------------------------------------------------------------------								
							elsif SEGMENT_SRC = '1' then -- if source 
								if RETRANSMIT = '1' then -- -- if corner  and source
									corner_flag <= '1';
									bus_out     <= bus_in; -- re-transmit routing intruction to the next node 
									if vertical = '0' then -- select the direction of re-transmission
										bus_ver_sel <= '1';
									else
										bus_hor_sel <= '1';
									end if;
								elsif flip_transmit = '1' then -- if it is intermediate node 
									bus_out                     <= bus_in;
									bus_out.INSTRUCTION(interm) <= not bus_in.INSTRUCTION(interm);
								else    -- normal source node
									next_state   <= WAIT4AGU;
									src_agu_flag <= '1';
								end if;
							----------------------------CORNER ONLY-----------------------------------------------------------------------------------------------
							elsif RETRANSMIT = '1' then -- if corner node
								corner_flag <= '1';
								if vertical = '0' then
									bus_ver_sel <= '1'; --if this is horizontal then restramsmit to vertical
								else
									bus_hor_sel <= '1'; -- if it is vertical then re-transmit to horizontal
								end if;
								bus_out <= bus_in; -- re-transmit the routing instruction to the relevant segment 

								if bus_in.instr_code = AGU_instruction then
									if i_ON = '1' and i_outputcontrol = '0' then -- source instruction 
										corner_flag <= '0';
									elsif i_ON = '0' and i_outputcontrol = '1' then -- destination instruction
										corner_flag <= '0';
									else
										next_state <= WAIT4AGU;
									end if;
								else
									next_state <= WAIT4AGU;
								end if;

								if flip_transmit = '1' then -- if it is an intermediate node flip the segment bit and retransmit
									bus_out                     <= bus_in;
									bus_out.INSTRUCTION(interm) <= not bus_in.INSTRUCTION(interm);
								else
									bus_out <= bus_in;
								end if;
							else        -- stright node 
								corner_flag <= '0';
								next_state  <= IDLE;
							end if;
----------------------------PARTITIONING-----------------------------------------------------------------------------------------------	
							if NORTH_instruction.ENABLE = '1' then -- open north path
								--if NORTH_instruction.PARTITION = north_splitter_direction then --check if path is already open
								--else    -- if not then open 
									bus_out     <= bus_in;
									bus_ver_sel <= '1';
									next_state   <= WAIT4AGU;

								--end if;
							end if;
							if SOUTH_instruction.ENABLE = '1' then -- open south path
								--if SOUTH_instruction.PARTITION = south_splitter_direction then --check if path is already open
								--else    -- if not then open 
									bus_out     <= bus_in;
									bus_ver_sel <= '1';
									next_state   <= WAIT4AGU;
								--end if;

							end if;
							if EAST_instruction.ENABLE = '1' then-- open east path
								--if EAST_instruction.PARTITION = east_splitter_direction then
								--else
									bus_out     <= bus_in;
									bus_hor_sel <= '1';
									turn_right  <= '1';
									next_state   <= WAIT4AGU;
								--end if;
							end if;
							if WEST_instruction.ENABLE = '1' then-- open west path
									bus_out     <= bus_in;
									bus_hor_sel <= '1';
									turn_left   <= '1';
									next_state   <= WAIT4AGU;
								--	assert false report "An WEST instruction recieved" severity error;
								-- if WEST_instruction.PARTITION = west_splitter_direction then
								-- 	bus_out     <= bus_in;
								-- 	bus_hor_sel <= '1';
								-- else
								-- 	bus_out     <= bus_in;
								-- 	bus_hor_sel <= '1';
								-- 	turn_left   <= '1';
								-- end if;
							end if;
----------------------------IGNORE/RE-TRANSANSMIT-----------------------------------------------------------------------------------------------	
						elsif bus_in.instr_code = AGU_instruction then
							next_state <= IDLE;
							if corner_flag = '1' then
								bus_out <= bus_in;
								if vertical = '0' then
									bus_ver_sel <= '1';
								else
									bus_hor_sel <= '1';
								end if;
							end if;     --corner
						elsif bus_in.instr_code = route_instruction then -- this is not followed by an agu instruction
							src_agu_flag <= '0';
							dst_agu_flag <= '0';
							----------------------------DESTINATION-----------------------------------------------------------------------------------------------  							
							if SEGMENT_DST = '1' then    -- if destination
								if RETRANSMIT = '1' then -- if corner node and destination node
									corner_flag <= '1';
									bus_out     <= bus_in;
									if vertical = '0' then -- define vertical or horizontal
										bus_ver_sel <= '1';
									else
										bus_hor_sel <= '1';
									end if;
								elsif flip_transmit = '1' then -- if it is intermediate node
									bus_out                     <= bus_in;
									bus_out.INSTRUCTION(interm) <= not bus_in.INSTRUCTION(interm);
								else    -- if it is just a normal destination node  then just wait for agu instruction																		
--									dst_agu_flag <= '1';
									next_state   <= IDLE;
								end if;
							----------------------------SOURCE-----------------------------------------------------------------------------------------------								
							elsif SEGMENT_SRC = '1' then -- if source 
								if RETRANSMIT = '1' then -- -- if corner  and source
									corner_flag <= '1';
									bus_out     <= bus_in; -- re-transmit routing intruction to the next node 
									if vertical = '0' then -- select the direction of re-transmission
										bus_ver_sel <= '1';
									else
										bus_hor_sel <= '1';
									end if;
								elsif flip_transmit = '1' then -- if it is intermediate node 
									bus_out                     <= bus_in;
									bus_out.INSTRUCTION(interm) <= not bus_in.INSTRUCTION(interm);
								else    -- normal source node
									next_state   <= IDLE;
--									src_agu_flag <= '1';
								end if;
							----------------------------CORNER ONLY-----------------------------------------------------------------------------------------------
							elsif RETRANSMIT = '1' then -- if corner node
								corner_flag <= '1';
								if vertical = '0' then
									bus_ver_sel <= '1'; --if this is horizontal then restramsmit to vertical
								else
									bus_hor_sel <= '1'; -- if it is vertical then re-transmit to horizontal
								end if;
								bus_out <= bus_in; -- re-transmit the routing instruction to the relevant segment 

--								if bus_in.instr_code = AGU_instruction then
--									if i_ON = '1' and i_outputcontrol = '0' then -- source instruction 
--										corner_flag <= '0';
--									elsif i_ON = '0' and i_outputcontrol = '1' then -- destination instruction
--										corner_flag <= '0';
--									else
--										next_state <= WAIT4AGU;
--									end if;
--								else
--									next_state <= WAIT4AGU;
--								end if;

								if flip_transmit = '1' then -- if it is an intermediate node flip the segment bit and retransmit
									bus_out                     <= bus_in;
									bus_out.INSTRUCTION(interm) <= not bus_in.INSTRUCTION(interm);
								else
									bus_out <= bus_in;
								end if;
							else        -- stright node 
								corner_flag <= '0';
								next_state  <= IDLE;
							end if;
							----------------------------PARTITIONING--------------------------------------------------------------	
							if NORTH_instruction.ENABLE = '1' then -- open north path
								--if NORTH_instruction.PARTITION = north_splitter_direction then --check if path is already open
								--else    -- if not then open 
									bus_out     <= bus_in;
									bus_ver_sel <= '1';
									next_state   <= WAIT4AGU;

								--end if;
							end if;
							if SOUTH_instruction.ENABLE = '1' then -- open south path
								--if SOUTH_instruction.PARTITION = south_splitter_direction then --check if path is already open
								--else    -- if not then open 
									bus_out     <= bus_in;
									bus_ver_sel <= '1';
									next_state   <= WAIT4AGU;
								--end if;

							end if;
							if EAST_instruction.ENABLE = '1' then-- open east path
								--if EAST_instruction.PARTITION = east_splitter_direction then
								--else
									bus_out     <= bus_in;
									bus_hor_sel <= '1';
									turn_right  <= '1';
									next_state   <= WAIT4AGU;
								--end if;
							end if;
							if WEST_instruction.ENABLE = '1' then-- open west path
									bus_out     <= bus_in;
									bus_hor_sel <= '1';
									turn_left   <= '1';
									next_state   <= WAIT4AGU;
								--	assert false report "An WEST instruction recieved" severity error;
								-- if WEST_instruction.PARTITION = west_splitter_direction then
								-- 	bus_out     <= bus_in;
								-- 	bus_hor_sel <= '1';
								-- else
								-- 	bus_out     <= bus_in;
								-- 	bus_hor_sel <= '1';
								-- 	turn_left   <= '1';
								-- end if;
							end if;        --instr_code
						end if;
					end if;             --bus enable
				when WAIT4AGU => if bus_in.instr_code = AGU_instruction then
						if corner_flag = '0' then
							if src_agu_flag = '1' then -- if it is source node 
								if  i_outputcontrol  = '0' then -- confirm it it is read insutrction
--									SRAM_AGU_instruction_r_cnct :=bus_in.INSTRUCTION&zero_halts;
									SRAM_AGU_instruction_r <= unpack_sram_noc_agu(bus_in.INSTRUCTION);--unsigned(SRAM_AGU_instruction_r_cnct);
									agu_en_r               <= '1';
									next_state             <= IDLE;
									src_agu_flag           <= '0';
								else    -- just ignore the instruction this is not for you. 
									-- maybe we might need to send it up if re-tramsit is there. 
									next_state <= WAIT4AGU;
								end if;
							elsif dst_agu_flag = '1' then -- if this is the destination node 
								if  i_outputcontrol  = '1' then -- recheck if it is write instruction
--									SRAM_AGU_instruction_w_cnct :=bus_in.INSTRUCTION&zero_halts;-- concatanate halt bits as all zeros 
									

									SRAM_AGU_instruction_w <= unpack_sram_noc_agu(bus_in.INSTRUCTION);
									agu_en_w               <= '1';
									next_state             <= IDLE;
									dst_agu_flag           <= '0';
								else    -- just ignore the instruction this is not for you. 
									-- maybe we might need to send it up if re-tramsit is there. 
									next_state <= WAIT4AGU;
								end if;

							else        -- it is an agu instruction but are neither source nor destination for it. 
								bus_out <=  bus_in;
								next_state <= IDLE;
								--assert false report "An agu instruction stright passed" severity error;
								if vertical = '0' then
									bus_ver_sel <= '1';
								else
									bus_hor_sel <= '1';
								end if;
							end if;
						else            -- if it is corner node which has recieved agu instruction then retransmit it and reset the corner node
							bus_out     <= bus_in;
							corner_flag <= '0'; -- reset the corner node as agu instruction has passed. 
							if vertical = '0' then
								bus_ver_sel <= '1';
							else
								bus_hor_sel <= '1';
							end if;
						end if;
					else
						next_state <= WAIT4AGU;
					end if;

			end case;
		end if;                         --clk
	end process p_source_fsm;

	current_state <= next_state;
end architecture RTL;
