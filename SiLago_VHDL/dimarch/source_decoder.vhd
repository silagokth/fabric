-------------------------------------------------------
--! @file
--! @brief source_decoder
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
-- Title      : source_decoder
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : source_decoder.vhd
-- Author     : Muhammad Adeel Tajammul
-- Company    : KTH
-- Created    : 
-- Last update: 2019-03-11
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2014 
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		          Description
--            1.0      Muhammad Adeel Tajammul   Created
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



LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
use ieee.NUMERIC_STD.all;
use work.crossbar_types_n_constants.all;
use ieee.numeric_std_unsigned.all;
--use work.drra_types_n_constants.all;
use work.noc_types_n_constants.all;

entity source_decoder is
	generic(
		This_ROW : std_logic_vector(ROW_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');
		This_COL : std_logic_vector(COL_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');
		VERTICAL : std_logic                                := '1');
	port(
		-- 		rst_n             : in	std_logic;
		-- 		clk               : in	std_logic;
		NOC_BUS_IN        : IN  NOC_BUS_TYPE;
		DIRECTION_OUT     : OUT CORSSBAR_INSTRUCTION_RECORD_TYPE;
		RETRANSMIT        : OUT STD_LOGIC;
		SEGMENT_SRC       : OUT STD_LOGIC;
		SEGMENT_DST       : OUT STD_LOGIC;
		flip_transmit     : OUT STD_LOGIC;
		--		rw                : OUT STD_LOGIC;
		NORTH_instruction : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
		SOUTH_instruction : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
		EAST_instruction  : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
		WEST_instruction  : OUT PARTITION_INSTRUCTION_RECORD_TYPE
	);
end entity source_decoder;

architecture RTL of source_decoder is
	SIGNAL RETRANSMIT_FLAG  : STD_LOGIC;
	SIGNAL SEGMENT_SRC_FLAG : STD_LOGIC;
	SIGNAL SEGMENT_DST_FLAG : STD_LOGIC;
	--SIGNAL		flip_transmit_FLAG : 	STD_LOGIC;		

	alias i_BUS_ENABLE : STD_LOGIC IS NOC_BUS_IN.bus_enable;
	alias i_instr_code : std_logic_vector(INS_WIDTH - 1 DOWNTO 0) IS NOC_BUS_IN.instr_code;

	-- PATH SETUP FLAGS

	--
	alias i_intermediate_node_flag    : std_logic IS NOC_BUS_IN.INSTRUCTION(INTERMEDIATE_NODE_FLAG_l); -- IF 1 THEN INTERMEDIATE node instruction 
	alias i_intermediate_segment_flag : std_logic IS NOC_BUS_IN.INSTRUCTION(INTERMEDIATE_SEGMENT_FLAG_l); -- IF 1 THEN INTERMEDIATE segment source to intermediate 


	-- ADDRESSES 
	alias i_SR         : std_logic_vector(ROW_WIDTH - 1 DOWNTO 0) IS NOC_BUS_IN.INSTRUCTION(SR_e DOWNTO SR_s); -- SOURCE ROW
	alias i_SC         : std_logic_vector(COL_WIDTH - 1 DOWNTO 0) IS NOC_BUS_IN.INSTRUCTION(SC_e DOWNTO SC_s); -- SOURCE COLUMN
	alias i_DR         : std_logic_vector(ROW_WIDTH - 1 DOWNTO 0) IS NOC_BUS_IN.INSTRUCTION(DR_e DOWNTO DR_s); -- DESTINATION ROW
	alias i_DC         : std_logic_vector(COL_WIDTH - 1 DOWNTO 0) IS NOC_BUS_IN.INSTRUCTION(DC_e DOWNTO DC_s); -- DESTINATION COLUMN
	alias i_IR         : std_logic_vector(ROW_WIDTH - 1 DOWNTO 0) IS NOC_BUS_IN.INSTRUCTION(IR_e DOWNTO IR_s); -- INTERMEDIATE ROW
	alias i_IC         : std_logic_vector(COL_WIDTH - 1 DOWNTO 0) IS NOC_BUS_IN.INSTRUCTION(IC_e DOWNTO IC_s); -- INTERMEDIATE COLUMN 
	--alias   i_PR    : STD_LOGIC 						IS NOC_BUS_IN.INSTRUCTION(PR_l);				-- PRIORITY
	alias I_ON         : STD_LOGIC IS NOC_BUS_IN.INSTRUCTION(ON_l); -- ORIGIN NODE -> 1=DESTINATION () 0 if source is origin 1 if destination is origin  )
	ALIAS i_union      : std_logic is NOC_BUS_IN.INSTRUCTION(UNION_FLAG_l);
	ALIAS i_union_port : std_logic_vector(1 downto 0) is NOC_BUS_IN.INSTRUCTION(UNION_PORT_e DOWNTO UNION_PORT_s); -- 


	signal DIRECTION : CORSSBAR_INSTRUCTION_RECORD_TYPE;

	signal                              --sr_dr,		sc_dc,
	mr_sr, mc_sc, mr_dr, mc_dc, mr_sr_eq, mc_sc_eq, mr_dr_eq, mc_dc_eq : std_logic;

	-- SIGNAL	DIFF_COL		: SIGNED (COL_WIDTH DOWNTO 0);
	-- SIGNAL 	ROW_ZERO_FLAG : STD_LOGIC;
	-- SIGNAL   COL_ZERO_FLAG : STD_LOGIC; 


	SIGNAL my_src_row_diff, my_des_row_diff : SIGNED(ROW_WIDTH DOWNTO 0);
	signal my_src_col_diff, my_des_col_diff : SIGNED(COL_WIDTH DOWNTO 0);

	signal src_locate : std_logic_vector(3 DOWNTO 0);
	signal dst_locate : std_logic_vector(3 DOWNTO 0);

begin
	-- src_dst_row_checking: process(i_SR, i_SC, i_DR, i_DC, I_ON) is
	-- begin

	-- 	if i_on = '1' THEN
	-- 		IF This_ROW-1 = i_DR THEN
	-- 			mr_dr_eq <= '1';
	-- 		ELSE 
	-- 			mr_dr_eq <= '0';
	-- 		END IF;

	-- 		IF  This_ROW = i_SR THEN
	-- 			mr_sr_eq <= '1';
	-- 		ELSE
	-- 			mr_sr_eq <= '0';
	-- 		END IF;

	-- 	else -- I_ON = 0, SRC will be Sequence Row and Column

	-- 		IF (This_ROW-1) = i_SR THEN
	-- 			mr_sr_eq <= '1';
	-- 		ELSE 
	-- 			mr_sr_eq <= '0';
	-- 		END IF;

	-- 		IF  This_ROW = i_DR THEN
	-- 			mr_dr_eq <= '1';
	-- 		ELSE
	-- 			mr_dr_eq <= '0';
	-- 		END IF;

	-- 	end if;
	-- end process src_dst_row_checking;

	mr_sr_eq <= '1' when This_ROW = i_SR else '0';
 	mr_dr_eq <= '1' when This_ROW = i_DR else '0';
	mc_sc_eq <= '1' when This_COL = i_SC else '0';
	mc_dc_eq <= '1' when This_COL = i_DC else '0';

	mr_sr <= my_src_row_diff( ROW_WIDTH ); -- if 1 then this mr < sr
	mr_dr <= my_des_row_diff( ROW_WIDTH ); -- if 1 then this mr < dr

	mc_sc <= my_src_col_diff( COL_WIDTH ); -- if 1 then this mc < sc
	mc_dc <= my_des_col_diff( COL_WIDTH ); -- if 1 then this mc < dr

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
-- purpose : subtractors
-- type    : Combinatorial
-- inputs  : i_SR, i_SC, i_DR, i_DC, i_intermediate_node_flag, NOC_BUS_IN.bus_enable
-- outputs : my_src_row_diff, my_src_col_diff, my_des_row_diff, my_des_col_diff

	
	p_my_diffs : process(i_SR, i_SC, i_DR, i_DC, i_intermediate_node_flag, NOC_BUS_IN.bus_enable) is
	begin

		my_src_row_diff <= (OTHERS => '0');
		my_src_col_diff <= (OTHERS => '0');
		my_des_row_diff <= (OTHERS => '0');
		my_des_col_diff <= (OTHERS => '0');

		if NOC_BUS_IN.bus_enable = '1' then -- IF VALID INSTRUCTION
			IF i_intermediate_node_flag = '1' THEN -- intermediate instruction Source --> Intermediate -->  Destination
				if (I_ON = '1' AND i_intermediate_segment_flag = '1') OR (I_ON = '0' AND i_intermediate_segment_flag = '0') THEN -- between source and intermediate
					my_src_row_diff <= SIGNED('0' & This_ROW) - SIGNED('0' & i_SR);
					my_src_col_diff <= SIGNED('0' & This_COL) - SIGNED('0' & i_SC);

					my_des_row_diff <= SIGNED('0' & This_ROW) - SIGNED(i_IR);
					my_des_col_diff <= SIGNED('0' & This_COL) - SIGNED(i_IC);
				ELSE                    -- between intermediate and destination
					my_src_row_diff <= SIGNED('0' & This_ROW) - SIGNED('0' & i_IR);
					my_src_col_diff <= SIGNED('0' & This_COL) - SIGNED('0' & i_IC);

					my_des_row_diff <= SIGNED('0' & This_ROW) - SIGNED('0' & i_DR);
					my_des_col_diff <= SIGNED('0' & This_COL) - SIGNED('0' & i_DC);
				END IF;
			ELSE                        -- direct instruction Source --> Destination

				my_src_row_diff <= SIGNED('0' & This_ROW) - SIGNED('0' & i_SR);
				my_src_col_diff <= SIGNED('0' & This_COL) - SIGNED('0' & i_SC);
				my_des_row_diff <= SIGNED('0' & This_ROW) - SIGNED('0' & i_DR);
				my_des_col_diff <= SIGNED('0' & This_COL) - SIGNED('0' & i_DC);

			--	if i_on = '1' THEN  -- DST is current SEQUENCER
			--		my_des_row_diff <= SIGNED(This_ROW-1) - SIGNED(i_DR);
			--	ELSE
			--		my_src_row_diff <= SIGNED(This_ROW-1) - SIGNED(i_SR);
			--	END IF;


			END IF;
		end IF;
	end process p_my_diffs;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
-- purpose : To identify the Source Cell
-- type    : Combinatorial
-- inputs  : i_SR, i_SC, i_DR, i_DC, NOC_BUS_IN.bus_enable
-- outputs : locate								
------------------------------------------------------------------------------------------------------------------------
	
	p_src_ident : process(mr_sr, mc_sc, mr_sr_eq, mc_sc_eq, NOC_BUS_IN.bus_enable) is
	--Assumed R2,C2 as the processing Node
	-- S -> SOURCE
	-- D -> DESTINATION   Col1		Col2	   Col3
	--					  |    	    |          |
	--			      --(3,1)-----(3,2)------(3,3)-- 	Row 3
	-- 					  |			|		   |
	--					  |	   Q4	|    Q1    |
	--					  |			|          |
	-- 				  --(2,1)-----(2,2)------(2,3)--- 	Row 2	
	-- 					  |			|          |
	--					  |	   Q3	|    Q2    |
	--			          |			|          |
	--			      --(1,1)-----(1,2)------(1,3)--- 	Row 1
	--                    |         |          |

	begin
		IF NOC_BUS_IN.bus_enable = '1' THEN
			IF mr_sr = '1' THEN 				    ---[ ]---[ ]---[S]---		R3	
				IF mc_sc = '1' THEN 			    ---[D]-[D/M]---[ ]---		R2
					src_locate <= Quad_3;      	    ---[D]---[D]---[ ]---		R1     

												    ---[ ]---[S]---[ ]---		R3	
				ELSIF mc_sc_eq = '1' THEN 		    ---[D]-[D/M]---[D]---		R2
					src_locate <= Quad_23;			---[D]---[D]---[D]---		R1     
												    
				ELSE 							    ---[S]---[ ]---[ ]---		R3
					src_locate <= Quad_2; 			---[ ]---[M/D]-[D]---		R2     
				END IF; 							---[ ]---[D]---[D]---		R1

			ELSIF mr_sr_eq = '1' THEN 				---[D]---[D]---[ ]---		R3	
				IF mc_sc = '1' THEN 		 		---[D]-[M/D]---[S]---		R2
					src_locate <= Quad_34;			---[D]---[D]---[ ]---		R1     

				ELSIF mc_sc_eq = '1' THEN 		 	---[D]---[D]---[D]---		R3			 
					src_locate <= Adjacents ;	    ---[D]--[S=M]--[D]---		R2
												    ---[D]---[D]---[D]---		R1

				ELSE 							    ---[ ]---[D]---[D]---		R3										
					src_locate <= Quad_12; 		    ---[S]---[M/D]-[D]---		R2
				END IF;							    ---[ ]---[D]---[D]---		R1

			ELSE   -- mr > sr 						---[D]---[D]---[ ]---		R3
				IF mc_sc = '1' THEN 			    ---[D]-[D/M]---[ ]---		R2	     
					src_locate <= Quad_4; 		    ---[ ]---[ ]---[S]---		R1												    

				ELSIF mc_sc_eq = '1' THEN 		    ---[D]---[D]---[D]---		R3    
					src_locate <= Quad_41; 		    ---[D]--[M/D]--[D]---		R2
												    ---[ ]---[S]---[ ]---		R1
				
				ELSE 							    ---[ ]---[D]---[D]---		R3
					src_locate <= Quad_1; 			---[ ]---[M/D]-[D]---		R2	   
				END IF;							    ---[S]---[ ]---[ ]---		R1

			END IF; 		-- Row Comparision BLOCK
		END IF; --NOC_BUS_IN.bus_enable = '1
	END process p_src_ident;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
-- purpose : To identify the destination cell
-- type    : Combinatorial
-- inputs  : i_SR, i_SC, i_DR, i_DC, NOC_BUS_IN.bus_enable
-- outputs : locate

	p_dst_ident : process(mr_dr, mc_dc, mr_dr_eq, mc_dc_eq, NOC_BUS_IN.bus_enable) is
	begin
		IF NOC_BUS_IN.bus_enable = '1' THEN
			IF mr_dr = '1' THEN 				---[ ]---[ ]---[D]---		R3	
				IF mc_dc = '1' THEN 			---[ ]---[M]---[ ]---		R2
					dst_locate <= NORTH_EAST;   ---[ ]---[ ]---[ ]---		R1     

												---[ ]---[D]---[ ]---		R3	
				ELSIF mc_dc_eq = '1' THEN 		---[ ]---[M]---[ ]---		R2
					dst_locate <= NORTH;		---[ ]---[ ]---[ ]---		R1          
												
				ELSE 							---[D]---[ ]---[ ]---		R3
					dst_locate <= NORTH_WEST;	---[ ]---[M]---[ ]---		R2     
				END IF; 						---[ ]---[ ]---[ ]---		R1

			ELSIF mr_dr_eq = '1' THEN 			---[ ]---[ ]---[ ]---		R3	
				IF mc_dc = '1' THEN 	 		---[ ]---[M]---[D]---		R2
					dst_locate <= EAST;			---[ ]---[ ]---[ ]---		R1     

				ELSIF mc_dc_eq = '1' THEN 		---[ ]---[ ]---[ ]---		R3			 
					dst_locate <= SAME;	        ---[ ]--[D=M]--[ ]---		R2
												---[ ]---[ ]---[ ]---		R1

				ELSE 							---[ ]---[ ]---[ ]---		R3										
					dst_locate <= WEST; 		---[D]---[M]---[ ]---		R2
				END IF;							---[ ]---[ ]---[ ]---		R1
			
			ELSE   -- mr > dr 					---[ ]---[ ]---[ ]---		R3
				IF mc_dc = '1' THEN 			---[ ]---[M]---[ ]---		R2    
					dst_locate <= SOUTH_EAST;	---[ ]---[ ]---[D]---		R1													

				ELSIF mc_dc_eq = '1' THEN 		---[ ]---[ ]---[ ]---		R3    
					dst_locate <= SOUTH; 		---[ ]---[M]---[ ]---		R2	
												---[ ]---[D]---[ ]---		R1
				
				ELSE 							---[ ]---[ ]---[ ]---		R3  
					dst_locate <= SOUTH_WEST;	---[ ]---[M]---[ ]---		R2	 
				END IF;							---[D]---[ ]---[ ]---		R1

			END IF; 		-- Row Comparision BLOCK
		END IF; --NOC_BUS_IN.bus_enable = '1
    END process p_dst_ident;
--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
-- purpose : Assigns signals to output ports
-- type    : Combinatorial
-- inputs  : i_intermediate_node_flag, i_intermediate_segment_flag, SEGMENT_SRC_FLAG, SEGMENT_DST_FLAG
-- outputs : NORTH_instruction, SOUTH_instruction , EAST_instruction, WEST_instruction, RETRANSMIT_FLAG
--			 SEGMENT_SRC_FLAG, SEGMENT_DST_FLAG, DIRECTION
	
u_flip_transmit : process(i_intermediate_node_flag, i_intermediate_segment_flag, SEGMENT_SRC_FLAG, SEGMENT_DST_FLAG) is
	begin
		flip_transmit <= '0';
		if i_intermediate_node_flag = '1' then
			if i_intermediate_segment_flag = '1' and SEGMENT_SRC_FLAG = '1' THEN
				flip_transmit <= '1';
			elsif i_intermediate_segment_flag = '0' and SEGMENT_DST_FLAG = '1' THEN
				flip_transmit <= '1';
			end if;
		end if;

end process u_flip_transmit;

	RETRANSMIT    <= RETRANSMIT_FLAG;
	SEGMENT_SRC   <= SEGMENT_SRC_FLAG;
	SEGMENT_DST   <= SEGMENT_DST_FLAG;
	DIRECTION_OUT <= DIRECTION;

--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
-- purpose : set the direction flags and NEWS instruction in order to route SRC and DST
-- type    : Combinatorial
-- inputs  : src_locate, dst_locate, NOC_BUS_IN  
-- outputs : NORTH_instruction, SOUTH_instruction , EAST_instruction, WEST_instruction, RETRANSMIT_FLAG
--			 SEGMENT_SRC_FLAG, SEGMENT_DST_FLAG, DIRECTION
-- note    : sram_read  =>  instruction will be issued by the dst node (i_on = 1), routing starts from the dst node
-- 			 sram_write =>  instruction will be issued by the src node (i_on = 0), routing starts from the src node
-- 			 vertical source decoder in top, bottom, left and right partitioning has higher priority then horizontal source decoder
-----------------------------------------------------------------------------------------------------------------------------------

p_inst_decoder : process(NOC_BUS_IN, src_locate, dst_locate) is
		--p_inst_decoder: process (clk,rst_n) is
	begin
		NORTH_instruction <= IDLE_PAR_INST;
		SOUTH_instruction <= IDLE_PAR_INST;
		EAST_instruction  <= IDLE_PAR_INST;
		WEST_instruction  <= IDLE_PAR_INST;
		RETRANSMIT_FLAG   <= '0';
		SEGMENT_SRC_FLAG  <= '0';
		SEGMENT_DST_FLAG  <= '0';
		DIRECTION         <= IGNORE;
		--status            := 0;
		-- I_ON = '0' -> DST Makes the connection
		-- I_ON = '1' -> SRC Makes the connection
		if i_BUS_ENABLE = '1' and unsigned(i_instr_code) /= unsigned(AGU_instruction) then -- IF VALID INSTRUCTION AND i_instr_code = both_instruction
			case  src_locate  IS
				when Quad_3  =>  														---[ ]---[ ]---[S]---		R3
						 																---[D]-[D/M]---[ ]---		R2
						case  dst_locate  IS											---[D]---[D]---[ ]---		R1 																		
							WHEN SAME => 												
								SEGMENT_DST_FLAG <= '1';								
								IF VERTICAL = '1' and I_ON = '1' THEN       			
									DIRECTION 			<= FROM_NORTH_TO_MEMORY; 		---[ ]---[ ]---[S]---		R3	
									NORTH_instruction 	<= LOW_UP_PAR_INST; 			---[ ]-[D/M]---[ ]---		R2 ( MR < SR, MC < SC, MR = DR, MC = DC)
								ELSIF VERTICAL = '0' and I_ON = '0' THEN 				---[ ]---[ ]---[ ]---		R1  case 5
									DIRECTION 			<= FROM_EAST_TO_MEMORY;
									EAST_instruction    <= LOW_RITE_PAR_INST;
								END IF;
																						
							WHEN WEST => 					 							---[ ]---[ ]---[S]---		R3	
								IF VERTICAL = '0' and I_ON = '0' THEN     			    ---[D]---[M]---[ ]---		R2 ( MR < SR, MC < SC, MR = DR, MC > DC)
									DIRECTION 			<= FROM_EAST_TO_WEST; 			---[ ]---[ ]---[ ]---		R1  case 6
									WEST_instruction 	<= LOW_LEFT_PAR_INST;
							--  ELSE
									-- if the instruction is issued by DST, according to priority rule, 
									-- vertical decoder will node will connect to Node(3,1). Node(2,2)
									-- can not be a valid Node, hence ignore checking I_ON = '1'
								END IF;
							
						    WHEN SOUTH =>  -- (2,2) to (1,2)				              ---[ ]---[ ]---[S]---		R3 
								IF VERTICAL = '1' THEN  								  ---[ ]---[M]---[ ]---		R2 ( MR < SR, MC < SC, MR > DR, MC = DC)
									DIRECTION 			    <= FROM_NORTH_TO_SOUTH;		  ---[ ]---[D]---[ ]---		R1 	case 8
									IF I_ON = '1' THEN									
										NORTH_instruction	<= LOW_UP_PAR_INST;           -- IF SRC MAKES CONNECTION, N(2,3) WOULD HAVE OPENED THE CONNECTION
									END IF;												  	
								END IF ;

							-- WHEN SOUTH_WEST =>   												 --(2,2) to (1,1)
							
							WHEN others => null ;

						END case;
				--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

				when Quad_23  =>  														---[ ]---[S]---[ ]---		R3						
				    	     															---[D]-[D/M]---[D]---		R2
						case  dst_locate IS 											---[D]---[D]---[D]---		R1		

							WHEN EAST => 					 												
								IF VERTICAL = '1'  THEN									---[ ]---[S]---[ ]---		R3	
									DIRECTION 			<= FROM_NORTH_TO_EAST;			---[ ]---[M]---[D]---		R2  ( MR < SR, MC = SC, MR = DR, MC < DC )
									RETRANSMIT_FLAG 	<= '1';							---[ ]---[ ]---[ ]---		R1   case 13
									IF I_ON = '0' 	THEN						
										NORTH_instruction	<= LOW_UP_PAR_INST;	
										EAST_instruction 	<= LOW_RITE_PAR_INST;
									END IF;
								END IF;							

							WHEN SAME => 												
								SEGMENT_DST_FLAG <= '1';								
								IF VERTICAL = '1'  THEN 								---[ ]---[S]---[ ]---		R3	
									DIRECTION 			<= FROM_NORTH_TO_MEMORY; 		---[ ]-[D/M]---[ ]---		R2 ( MR < SR, MC = SC, MR = DR, MC = DC )
									NORTH_instruction 	<= LOW_UP_PAR_INST;				---[ ]---[ ]---[ ]---		R1  case 14		
								END IF;

							WHEN WEST => 					 							
								IF VERTICAL = '1'  AND I_ON = '0' THEN 					---[ ]---[S]---[ ]---		R3	
									DIRECTION 			<= FROM_NORTH_TO_WEST; 			---[D]---[M]---[ ]---		R2 ( MR < SR, MC = SC, MR = DR, MC > DC )
									RETRANSMIT_FLAG 	<= '1'; 						---[ ]---[ ]---[ ]---		R1  case 15
									NORTH_instruction	<= LOW_UP_PAR_INST;	
									WEST_instruction 	<= LOW_LEFT_PAR_INST;
								END IF;			

							WHEN SOUTH_WEST | SOUTH | SOUTH_EAST =>   						    	
								IF VERTICAL = '1' THEN
									DIRECTION 			<= FROM_NORTH_TO_SOUTH; 		---[ ]---[S]---[ ]---		R3  ( MR < SR, MC = SC, MR > DR, MC < DC ) case 16
									IF I_ON = '0' THEN									---[ ]---[M]---[ ]---		R2	( MR < SR, MC = SC, MR > DR, MC = DC ) case 17		
										NORTH_instruction 	<= LOW_DOWN_PAR_INST;       ---[D]---[D]---[D]---		R1  ( MR < SR, MC = SC, MR > DR, MC > DC ) case 18
									ELSIF  I_ON = '1' AND dst_locate = SOUTH  THEN
										NORTH_instruction  	<= LOW_UP_PAR_INST;
									END IF;				
								-- SOUTH_EAST AND SOUTH_WEST WITH I_ON=1 CAN NOT HAVE NODE(2,2) AS INTERMEDIATE PATH
								-- Dont have to check for horizontal, assuming path would not have multiple steps
								END IF ;									    

							WHEN others => null ;

						END case;
				--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	

				when Quad_2 =>														---[S]---[ ]---[ ]---		R3
																					---[ ]---[M/D]-[D]---		R2					
					case  dst_locate  IS											---[ ]---[D]---[D]---		R1 

						WHEN EAST => 					 							---[S]---[ ]---[ ]---		R3
							IF VERTICAL = '0' and I_ON = '0' THEN 					---[ ]---[M]---[D]---		R2  ( MR < SR, MC > SC, MR = DR, MC < DC )
								DIRECTION 			<= FROM_WEST_TO_EAST;			---[ ]---[ ]---[ ]---		R1  case 22
								EAST_instruction 	<= LOW_RITE_PAR_INST;
							END IF;

						WHEN SAME => 																				
							SEGMENT_DST_FLAG <= '1';								
							IF VERTICAL = '1' and I_ON = '1' THEN 					
								DIRECTION 			<= FROM_NORTH_TO_MEMORY;		---[S]---[ ]---[ ]---		R3
								NORTH_instruction 	<= LOW_UP_PAR_INST; 			---[ ]-[D/M]---[ ]---		R2 	( MR < SR, MC > SC, MR = DR, MC = DC )
							ELSIF VERTICAL = '0' and I_ON = '0' THEN 				---[ ]---[ ]---[ ]---		R1  case 23
								DIRECTION 			<= FROM_WEST_TO_MEMORY;  		-- case 23
								--WEST_instruction    <= LOW_LEFT_PAR_INST;         -- Node (2,1) would already opened up
							END IF;
								
					    WHEN SOUTH =>        						    	 
							IF VERTICAL = '1'  THEN 								---[S]---[ ]---[ ]---		R3
								DIRECTION 			<= FROM_NORTH_TO_SOUTH;			---[ ]---[M]---[ ]---		R2  (MR < SR, MC > SC, MR > DR, MC = DC)
								IF I_ON = '1' THEN 									---[ ]---[D]---[ ]---		R1  case 26
									NORTH_instruction 	<= LOW_UP_PAR_INST;
								END IF;			
							END IF;
											
						WHEN others => null ;

					END case;
				--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	 

				WHEN Quad_34 =>														---[D]---[D]---[ ]---		R3								
																					---[D]-[M/D]---[S]---		R2 
					case  dst_locate    IS 											---[D]---[D]---[ ]---		R1  

						WHEN NORTH => 	
							IF VERTICAL = '1' THEN
								DIRECTION 			<= FROM_EAST_TO_NORTH; 			---[ ]---[D]---[ ]---		R3	
								RETRANSMIT_FLAG 	<= '1'; 						---[ ]---[M]---[S]---		R2  ( MR = SR, MC < SC, MR < DR, MC = DC )
								IF I_ON = '1'  THEN 								---[ ]---[ ]---[ ]---		R1  case 29
									NORTH_instruction	<= LOW_UP_PAR_INST;    			
									EAST_instruction 	<= LOW_RITE_PAR_INST;
								END IF;
							END IF;

						WHEN NORTH_WEST  => 										-- only dst can issue instruction		 					        
					        IF VERTICAL = '0'	THEN 			  					---[D]---[ ]---[ ]---		R3	( MR = SR, MC < SC, MR < DR, MC > DC ) 
					         	DIRECTION 			<=	FROM_EAST_TO_WEST; 			---[ ]---[M]---[S]---		R2  Case 30
					         	IF I_ON = '1' THEN         							---[ ]---[ ]---[ ]---		R1  
					         		EAST_instruction 	<= 	LOW_RITE_PAR_INST; 	
					         	END IF;
					        END IF;

						WHEN SAME =>							
							SEGMENT_DST_FLAG <= '1';
							DIRECTION 			<= FROM_EAST_TO_MEMORY;				---[ ]---[ ]---[ ]---		R3	
							IF VERTICAL = '1' and I_ON = '1' THEN 					---[ ]---[M/D]-[S]---		R2  ( MR = SR, MC < SC, MR = DR, MC = DC )	
								EAST_instruction    <= LOW_RITE_PAR_INST; 			---[ ]---[ ]---[ ]---		R1  case 32
							END IF;													-- IF DST MAKES CONNECTION, ONLY THEN CONFIGURE EAST INSTRUCTION
																					-- ELSE N(2,3) WOULD HAVE CONFIGURED IT ALREADY
																			
						WHEN  WEST => 									
					        IF VERTICAL = '0'	THEN 			  					---[ ]---[ ]---[ ]---		R3	
					         	DIRECTION 			<=	FROM_EAST_TO_WEST; 			---[D]---[M]---[S]---		R2  ( MR = SR, MC < SC, MR = DR, MC > DC ) 
					         	IF I_ON = '1' THEN  								---[ ]---[ ]---[ ]---		R1  Case 33
					         		EAST_instruction 	<= 	LOW_RITE_PAR_INST; 		
					         	ELSE 
					         		WEST_instruction	<= LOW_LEFT_PAR_INST;
					         	END IF;
					        END IF;	

						WHEN SOUTH => --???																		    
							IF VERTICAL = '1' AND I_ON = '1' THEN
								DIRECTION 			<= FROM_EAST_TO_SOUTH; 			---[ ]---[ ]---[S]---		R2	
								RETRANSMIT_FLAG 	<= '1'; 						---[ ]---[M]---[ ]---		R1	 ( MR = SR, MC < SC, MR > DR, MC = DC )
								--SOUTH_instruction	<= LOW_DOWN_PAR_INST; 			---[ ]---[D]---[ ]---		R1   case 35
								EAST_instruction 	<= LOW_RITE_PAR_INST;					
							END IF;

						WHEN  SOUTH_WEST => 									-- only dst can issue instruction		 					        
					        IF VERTICAL = '0'	THEN 			  					---[ ]---[ ]---[ ]---		R3	
					         	DIRECTION 			<=	FROM_EAST_TO_WEST; 			---[ ]---[M]---[S]---		R2  ( MR = SR, MC < SC, MR > DR, MC > DC ) 
					         	IF I_ON = '1' THEN 									---[D]---[ ]---[ ]---		R1  case 36
					         		EAST_instruction 	<= 	LOW_RITE_PAR_INST; 		
					         	--ELSIF I_ON = '0' 
					         	--	WEST_instruction	<= LOW_LEFT_PAR_INST;
					         	END IF;
					        END IF;

						WHEN others => null;

					END CASE;

   				--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	 
   				 	 			 
					WHEN ADJACENTS => 												---[D]---[D]---[D]---		R3							
																					---[D]--[S=M]--[D]---		R2
						case dst_locate  IS 										---[D]---[D]---[D]---		R1	

							WHEN  NORTH_EAST  => 				
								SEGMENT_SRC_FLAG 	<= '1'; 						
								IF VERTICAL = '1'  THEN 							---[ ]---[ ]---[D]---		R3	
									DIRECTION 			<= FROM_MEMORY_TO_NORTH;	---[ ]--[S=M]--[ ]---		R2  ( MR = SR, MC = SC, MR < DR, MC < DC )
									IF I_ON = '0'	THEN 							---[ ]---[ ]---[ ]---		R1   case 37
										NORTH_instruction	<= LOW_UP_PAR_INST;		
									END IF;
								ELSE 
									DIRECTION 			<= FROM_MEMORY_TO_EAST;
								END IF;

							WHEN NORTH =>
								SEGMENT_SRC_FLAG 	<= '1'; 
								IF VERTICAL = '1' THEN 								---[ ]---[D]---[ ]---		R3
									DIRECTION 			<= FROM_MEMORY_TO_NORTH;	---[ ]--[S=M]--[ ]---		R2  ( MR = SR, MC = SC, MR < DR, MC = DC ) 
									NORTH_instruction   <= 	LOW_UP_PAR_INST;		---[ ]---[ ]---[ ]---		R1  case 38
								END IF;

							WHEN NORTH_WEST => 											
								IF VERTICAL = '1' THEN 								---[D]---[ ]---[ ]---		R3		 		
									DIRECTION 			<= FROM_MEMORY_TO_NORTH; 	---[ ]--[S=M]--[ ]---		R2  ( MR = SR, MC = SC, MR < DR, MC > DC ) 
									IF  I_ON = '0'  THEN 							---[ ]---[ ]---[ ]---		R1  case 39	
										NORTH_instruction	<= LOW_UP_PAR_INST; 	
									END IF;
								ELSE 
									DIRECTION 			<= FROM_MEMORY_TO_WEST;
						        END IF;

						    WHEN EAST  =>
						        SEGMENT_SRC_FLAG <= '1';
						        DIRECTION 			<= FROM_MEMORY_TO_EAST;			---[ ]---[ ]---[ ]---		R3	( MR = SR, MC = SC, MR = DR, MC < DC ) 
						        IF VERTICAL = '1'  AND I_ON = '0' THEN 				---[ ]--[S=M]--[D]---		R2  case 40
					        		EAST_instruction	<= LOW_RITE_PAR_INST;  	    ---[ ]---[ ]---[ ]---		R1 
					        	END IF;

						    WHEN WEST =>
						        SEGMENT_SRC_FLAG <= '1';
				         		DIRECTION 			<= FROM_MEMORY_TO_WEST;     	 ---[ ]---[ ]---[ ]---		R3	
			        			IF VERTICAL = '1' AND I_ON = '0' THEN 				 ---[D]--[S=M]--[ ]---		R2  ( MR = SR, MC = SC, MR = DR, MC > DC ) 
				        		 	WEST_instruction	<= LOW_LEFT_PAR_INST; 		 ---[ ]---[ ]---[ ]---		R1  case 42
				        		END IF;
				        		

						    WHEN SOUTH_EAST => 				
								SEGMENT_SRC_FLAG 	<= '1'; --??? should we consider i_ON
								IF VERTICAL = '1' THEN 				 			  	    ---[ ]---[ ]---[ ]---		R3	( MR = SR, MC = SC, MR > DR, MC < DC )		
									DIRECTION 			<= FROM_MEMORY_TO_SOUTH;		---[ ]--[S=M]--[ ]---		R2  case 43	
								ELSIF VERTICAL = '0'  THEN 								---[ ]---[ ]---[D]---		R1  
									DIRECTION 			<= FROM_MEMORY_TO_EAST;	 		
								END IF;	

							WHEN SOUTH => 				--????
								SEGMENT_SRC_FLAG 	<= '1'; 
								IF VERTICAL = '1' THEN 								    ---[ ]---[ ]---[ ]---		R3	case 44		
									DIRECTION 			<= FROM_MEMORY_TO_SOUTH; 		---[ ]--[S=M]--[ ]---		R2  ( MR = SR, MC = SC, MR > DR, MC = DC )		 		
								END IF;													---[ ]---[D]---[ ]---		R1  
									
							

						    WHEN SOUTH_WEST  => 	--???			 should consider i_on?
								SEGMENT_SRC_FLAG 	<= '1'; 						
								IF VERTICAL = '1' THEN 									---[ ]---[ ]---[ ]---		R3
									DIRECTION 			<= FROM_MEMORY_TO_SOUTH; 		---[ ]--[S=M]--[ ]--- 		R2 ( MR = SR, MC = SC, MR > DR, MC > DC )		 		
								ELSIF VERTICAL = '0'   THEN 							---[D]---[ ]---[ ]--- 		R1 case 45
									DIRECTION 			<= FROM_MEMORY_TO_WEST;
								END IF;
							
							WHEN OTHERS => NULL;

						END CASE;

                --§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	 

				 							    									
					WHEN Quad_12 => 		       									---[ ]---[D]---[D]---		R3	
					    															---[S]---[M/D]-[D]---		R2
						case  dst_locate  IS 										---[ ]---[D]---[D]---		R1

							WHEN NORTH_EAST =>
								IF VERTICAL = '0'  AND I_ON = '1' THEN  			---[ ]---[ ]---[D]---		R3  ( MR = SR, MC > SC, MR < DR, MC < DC ) 
						         	DIRECTION 			<=	FROM_WEST_TO_EAST; 		---[S]---[M]---[ ]---		R2 	case 46
						         	WEST_instruction 	<= 	LOW_LEFT_PAR_INST;      ---[ ]---[ ]---[ ]---		R1 						        		
					       		END IF;

							WHEN NORTH =>			                
						        IF VERTICAL = '1'  THEN
									DIRECTION 			<= FROM_WEST_TO_NORTH; 		---[ ]---[D]---[ ]---		R3	
									RETRANSMIT_FLAG 	<= '1'; 					---[S]---[M]---[ ]---		R2	( MR = SR, MC > SC, MR < DR, MC = DC )
									IF I_ON = '1' THEN 								---[ ]---[ ]---[ ]---		R1  case 47
										NORTH_instruction	<= LOW_UP_PAR_INST;
										WEST_instruction 	<= LOW_LEFT_PAR_INST; 
									END IF;	
								END IF;

							WHEN EAST  =>								        
						        IF VERTICAL = '0' THEN  							---[ ]---[ ]---[ ]---		R3  
						         	DIRECTION 			<=	FROM_WEST_TO_EAST; 		---[S]---[M]---[D]---		R2 	( MR = SR, MC > SC, MR = DR, MC < DC ) 
						         	IF I_ON = '1' THEN 	                            ---[ ]---[ ]---[ ]---		R1  case 49
						         		WEST_instruction 	<= 	LOW_LEFT_PAR_INST; 
						         	ELSE
						         		EAST_instruction	<= LOW_RITE_PAR_INST;
						         	END IF;		
					       		END IF;

							WHEN SAME =>       
						        SEGMENT_DST_FLAG <= '1';
						        IF VERTICAL = '1' AND I_ON = '1' THEN 				---[ ]---[ ]---[ ]---		R3	
						        	DIRECTION 			<= FROM_WEST_TO_MEMORY;		---[S]---[M/D]-[ ]---		R2	( MR = SR, MC > SC, MR = DR, MC = DC )
						        	WEST_instruction 	<= LOW_LEFT_PAR_INST;		---[ ]---[ ]---[ ]---		R1  case 50
						        ELSIF VERTICAL = '0' AND I_ON = '0' THEN
						        	DIRECTION 			<= FROM_WEST_TO_MEMORY;
						        END IF;

						    WHEN  SOUTH_EAST =>								        
						        IF VERTICAL = '0' THEN  							---[ ]---[ ]---[ ]---		R3  
						         	DIRECTION 			<=	FROM_WEST_TO_EAST; 		---[S]---[M]---[ ]---		R2 	case 52
						         	IF I_ON = '1' THEN 				                ---[ ]---[ ]---[D]---		R1  ( MR = SR, MC > SC, MR > DR, MC < DC )
						         		WEST_instruction 	<= 	LOW_LEFT_PAR_INST; 
						         	--ELSE  not a valid case
						         	--	EAST_instruction	<= LOW_RITE_PAR_INST;  --???
						         	END IF;		
					       		END IF;

							WHEN SOUTH =>
						        IF VERTICAL = '1' AND I_ON = '1' THEN
									DIRECTION 			<= FROM_WEST_TO_SOUTH; 		---[ ]---[ ]---[ ]---		R3
									RETRANSMIT_FLAG 	<= '1'; 					---[S]---[M]---[ ]---		R2	( MR = SR, MC > SC, MR > DR, MC = DC )
									--SOUTH_instruction	<= LOW_DOWN_PAR_INST; 		---[ ]---[D]---[ ]---		R1
									WEST_instruction 	<= LOW_LEFT_PAR_INST;       -- case 53
							END IF;

							

						    WHEN OTHERS => NULL;

						END CASE;

                --§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	 

					WHEN Quad_4 =>		    										---[D]---[D]---[ ]---		R3 					 		
																					---[D]-[D/M]---[ ]---		R2
						case dst_locate IS 											---[ ]---[ ]---[S]---		R1
							WHEN NORTH =>  
					 	        IF VERTICAL = '1' THEN 								---[ ]---[D]---[ ]---		R3
									DIRECTION 			<= FROM_SOUTH_TO_NORTH; 	---[ ]---[M]---[ ]---		R2	( MR > SR, MC < SC, MR < DR, MC = DC )
									IF  I_ON = '1' THEN							    ---[ ]---[ ]---[S]---		R1  case 56
										 NORTH_instruction	<= LOW_UP_PAR_INST; 
									END IF;
								END IF;

							WHEN SAME =>
						        SEGMENT_DST_FLAG <= '1'; --?? shouldnt consider i_on ? 
								IF VERTICAL = '1'  THEN 				      		---[ ]---[ ]---[ ]---		R3     	
									DIRECTION 			<= FROM_SOUTH_TO_MEMORY; 	---[ ]-[D/M]---[ ]---		R2 	( MR > SR, MC < SC, MR = DR, MC = DC )
									--SOUTH_instruction 	<= LOW_DOWN_PAR_INST; 	---[ ]---[ ]---[S]---		R1  case 59
								ELSIF VERTICAL = '0'  THEN
									DIRECTION 			<= FROM_EAST_TO_MEMORY;
									--EAST_instruction    <= LOW_RITE_PAR_INST; 				
								END IF;

							WHEN WEST  =>
					 	        IF VERTICAL = '0'  THEN 							---[ ]---[ ]---[ ]---		R3 
									DIRECTION 			<= FROM_EAST_TO_WEST;		---[D]---[M]---[ ]---		R2  ( MR > SR, MC < SC, MR = DR, MC > DC )
									IF I_ON = '0' THEN 								---[ ]---[ ]---[S]---		R1	case 60
										WEST_instruction 	<= LOW_LEFT_PAR_INST;
									--ELSIF I_ON = '1' THEN
									--	EAST_instruction	<= LOW_RITE_PAR_INST;   --???
									END IF;		
								END IF;

							WHEN OTHERS => NULL;

						END CASE;

				--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	 
				 		    
					WHEN Quad_41 =>													---[D]---[D]---[D]---		R3 
																					---[D]--[M/D]--[D]---		R2
						case dst_locate IS 											---[ ]---[S]---[ ]---		R1
							
						    WHEN NORTH_EAST => 	
								IF VERTICAL = '1' THEN 								---[ ]---[ ]---[D]---		R3	( MR > SR, MC = SC, MR < DR, MC < DC)	
									DIRECTION 			<= FROM_SOUTH_TO_NORTH; 	---[ ]---[M]---[ ]---		R2	CASE 64
									IF I_ON = '0' THEN								---[ ]---[S]---[ ]---		R1  
										NORTH_instruction 	<= LOW_UP_PAR_INST; 	
									END IF;											
								 END IF ;
							WHEN NORTH => 	
								IF VERTICAL = '1' THEN 								---[ ]---[D]---[ ]---		R3		
									DIRECTION 			<= FROM_SOUTH_TO_NORTH; 	---[ ]---[M]---[ ]---		R2	( MR > SR, MC = SC, MR < DR, MC = DC)
									NORTH_instruction 	<= LOW_UP_PAR_INST; 		---[ ]---[S]---[ ]---		R1  CASE 65
								END IF;

							WHEN NORTH_WEST => 	
								IF VERTICAL = '1' and I_ON = '0' THEN 				---[D]---[ ]---[ ]---		R3	( MR > SR, MC = SC, MR < DR, MC > DC)	
									DIRECTION 			<= FROM_SOUTH_TO_NORTH; 	---[ ]---[M]---[ ]---		R2	CASE 66
									NORTH_instruction 	<= LOW_UP_PAR_INST; 		---[ ]---[S]---[ ]---		R1  
								END IF ;

							WHEN EAST => 					 				 					
								IF VERTICAL = '1' AND I_ON = '0'  THEN		 
									DIRECTION 			<= FROM_SOUTH_TO_EAST;		---[ ]---[ ]---[ ]---		R3	
									RETRANSMIT_FLAG 	<= '1';						---[ ]---[M]---[D]---		R2  ( MR > SR, MC = SC, MR = DR, MC < DC)	
								    EAST_instruction 	<= LOW_RITE_PAR_INST;	    ---[ ]---[S]---[ ]---		R1	CASE 67	
								END IF;


							WHEN SAME => 									
								SEGMENT_DST_FLAG <= '1';					
								IF VERTICAL = '1'  THEN 							---[ ]---[ ]---[ ]---		R3
									DIRECTION 			<= FROM_SOUTH_TO_MEMORY;	---[ ]-[D/M]---[ ]---		R2 ( MR > SR, MC = SC, MR = DR, MC = DC)
									--SOUTH_instruction 	<= LOW_DOWN_PAR_INST;	---[ ]---[S]---[ ]---		R1	CASE 68			
								END IF;
																			
							WHEN WEST => 					 				
								IF VERTICAL = '1'  AND I_ON = '0' THEN      
									DIRECTION 			<= FROM_SOUTH_TO_WEST; 		---[ ]---[ ]---[ ]---		R3	
									RETRANSMIT_FLAG 	<= '1'; 					---[D]---[M]---[ ]---		R2 	( MR > SR, MC = SC, MR = DR, MC > DC)
								--  SOUTH_instruction	<= LOW_DOWN_PAR_INST;		---[ ]---[S]---[ ]---		R1  case 69
									WEST_instruction 	<= LOW_LEFT_PAR_INST;
								END IF;
										
							WHEN OTHERS => NULL;

						END CASE;
				--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	 
				
					WHEN QUAD_1 =>													---[ ]---[D]---[D]---		R3
				        															---[ ]---[M/D]-[D]---		R2	
						case dst_locate   IS 										---[S]---[ ]---[ ]---		R1
							WHEN NORTH =>
					 	        IF VERTICAL = '1' AND I_ON = '1' THEN 				---[ ]---[D]---[ ]---		R3   
									DIRECTION 			<= FROM_SOUTH_TO_NORTH; 	---[ ]---[M]---[ ]---		R2 	( MR > SR, MC > SC, MR < DR, MC = DC)
									NORTH_instruction	<= LOW_UP_PAR_INST; 		---[S]---[ ]---[ ]---		R1  CASE 74
								END IF;

							WHEN EAST  =>
					 	        IF VERTICAL = '0' and I_ON = '0' THEN 				---[ ]---[ ]---[ ]---		R3  
									DIRECTION 			<= FROM_WEST_TO_EAST; 		---[ ]---[M]---[D]---		R2	( MR > SR, MC > SC, MR = DR, MC < DC)
									EAST_instruction 	<= LOW_RITE_PAR_INST;		---[S]---[ ]---[ ]---		R1	case 76
								END IF;

							WHEN SAME =>
						        SEGMENT_DST_FLAG <= '1';
								IF VERTICAL = '1' and I_ON = '1' THEN       		---[ ]---[ ]---[ ]---		R3    
									DIRECTION 			<= FROM_SOUTH_TO_MEMORY; 	---[ ]-[D/M]---[ ]---		R2	( MR > SR, MC > SC, MR = DR, MC = DC)
								--	SOUTH_instruction 	<= LOW_DOWN_PAR_INST;--???	---[S]---[ ]---[ ]---		R1  CASE 77
								ELSIF VERTICAL = '0' and I_ON = '0' THEN
									DIRECTION 			<= FROM_WEST_TO_MEMORY;
								--	WEST_instruction    <= LOW_LEFT_PAR_INST; --???
								END IF;

							

							

							WHEN OTHERS => NULL;

						END CASE;

					WHEN OTHERS => NULL ; 
				--§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§	 
			END CASE;	-- SRC_LOCATE
				
		end if; -- bus enable

end process p_inst_decoder;

end architecture RTL;