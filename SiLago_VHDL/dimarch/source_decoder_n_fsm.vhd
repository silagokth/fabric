-------------------------------------------------------
--! @file
--! @brief source_decoder_n_fsm
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
-- Title      : source_decoder_n_fsm
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : source_decoder_n_fsm.vhd
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use work.top_consts_types_package.SRAM_AGU_INSTR_WIDTH;
use work.crossbar_types_n_constants.CORSSBAR_INSTRUCTION_RECORD_TYPE;

use work.noc_types_n_constants.all;

entity source_decoder_n_fsm is
	generic ( 	
	This_ROW		: UNSIGNED (ROW_WIDTH-1 DOWNTO 0) :=(OTHERS => '0');
	This_COL		: UNSIGNED (COL_WIDTH-1 DOWNTO 0) :=(OTHERS => '0');
	vertical 		: std_logic 					  := '1');

	port (
		clk : in std_logic;
		rst_n : in std_logic;
		bus_in : in NOC_BUS_TYPE;
		
		east_splitter_direction,west_splitter_direction,north_splitter_direction,south_splitter_direction : in std_logic_vector (1 downto 0);
		
		bus_out : out NOC_BUS_TYPE;
		bus_ver_sel,bus_hor_sel : out std_logic;
		turn_left, turn_right : out std_logic;
		agu_en_r,agu_en_w : out std_logic;
		SRAM_AGU_instruction_r :out sram_agu_instruction_type;
		SRAM_AGU_instruction_w :out sram_agu_instruction_type;
		north_instruction_out,SOUTH_instruction_out,east_instruction_out,west_instruction_out : out PARTITION_INSTRUCTION_RECORD_TYPE;
		
		direction: out CORSSBAR_INSTRUCTION_RECORD_TYPE
	);
end entity source_decoder_n_fsm;

architecture RTL of source_decoder_n_fsm is
	signal RETRANSMIT_FLAG : STD_LOGIC;
	signal SEGMENT_SRC_FLAG : STD_LOGIC;
	signal SEGMENT_DST_FLAG : STD_LOGIC;
	signal flip_transmit : STD_LOGIC;
	signal north_instruction,SOUTH_instruction,east_instruction,west_instruction : PARTITION_INSTRUCTION_RECORD_TYPE;
--	signal turn_left : std_logic;
--	signal turn_right : std_logic;
begin
	u_location_decoder : entity work.source_decoder(RTL)
	generic map (		This_ROW => std_logic_vector(This_ROW),		This_COL => std_logic_vector(This_COL),		VERTICAL => vertical		)
	port map (

--		rst_n         => rst_n,
--		clk           => clk,
--		rw => rw,
		NOC_BUS_IN    => bus_in,
		DIRECTION_OUT => direction,
		RETRANSMIT    => RETRANSMIT_FLAG,
		SEGMENT_SRC   => SEGMENT_SRC_FLAG,
		SEGMENT_DST   => SEGMENT_DST_FLAG,
		flip_transmit => flip_transmit,
		NORTH_instruction => north_instruction,   
		SOUTH_instruction => SOUTH_instruction,
		EAST_instruction  =>  east_instruction, 
		WEST_instruction  =>  west_instruction 
		);

	u_source_fsm: entity work.source_fsm(RTL)
	generic map ( vertical => '1')
	port map (
		rst_n                  => rst_n,
		clk                    => clk,
		bus_in                 => bus_in,
		RETRANSMIT             => RETRANSMIT_FLAG,
		SEGMENT_SRC            => SEGMENT_SRC_FLAG,
		SEGMENT_DST            => SEGMENT_DST_FLAG,
		flip_transmit          => flip_transmit,
		NORTH_instruction => north_instruction,
		SOUTH_instruction => SOUTH_instruction,
		EAST_instruction  =>  east_instruction,
		WEST_instruction  =>  west_instruction,	
		east_splitter_direction => east_splitter_direction ,
		west_splitter_direction => west_splitter_direction ,
		north_splitter_direction=> north_splitter_direction,
		south_splitter_direction=> south_splitter_direction,
		bus_out                => bus_out,
		bus_ver_sel            => bus_ver_sel,
		bus_hor_sel            => bus_hor_sel,
		turn_left  => turn_left,
		turn_right  => turn_right,
		agu_en_r               => agu_en_r,
		agu_en_w               => agu_en_w,
		SRAM_AGU_instruction_r => SRAM_AGU_instruction_r,
		SRAM_AGU_instruction_w => SRAM_AGU_instruction_w
		);	

	north_instruction_out <= north_instruction;
	SOUTH_instruction_out <= SOUTH_instruction;
	east_instruction_out <= east_instruction;
	west_instruction_out <= west_instruction;
	
	
	

end architecture RTL;
