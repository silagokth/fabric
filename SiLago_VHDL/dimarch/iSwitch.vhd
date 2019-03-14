-------------------------------------------------------
--! @file
--! @brief iSwitch
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
-- Title      : iSwitch
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : iSwitch.vhd
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
--             1.0      Muhammad Adeel Tajammul   Created
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	   #
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




library IEEE;
use IEEE.std_logic_1164.all;
use ieee.NUMERIC_STD.all;
--use work.top_consts_types_package.SRAM_AGU_INSTR_WIDTH;
use work.crossbar_types_n_constants.all;
--use work.drra_types_n_constants.all;
--use work.SINGLEPORT_SRAM_AGU_types_n_constants.AGU_INSTR_WIDTH;
use work.noc_types_n_constants.all;

entity iSwitch is
	generic(
		This_ROW : UNSIGNED(ROW_WIDTH - 1 downto 0) := (others => '0');
		This_COL : UNSIGNED(COL_WIDTH - 1 downto 0) := (others => '0')
	);
	port(
		rst_n                    : in  std_logic;
		clk                      : in  std_logic;
		----------------------------
		--	partition_status 
		----------------------------
		east_splitter_direction  : in  std_logic_vector(1 downto 0);
		west_splitter_direction  : in  std_logic_vector(1 downto 0);
		north_splitter_direction : in  std_logic_vector(1 downto 0);
		south_splitter_direction : in  std_logic_vector(1 downto 0);
		----------------------------
		--	SEMENTED BUS I/0  
		----------------------------

		NORTH_BUS_OUT            : out NOC_BUS_TYPE;
		SOUTH_BUS_OUT            : out NOC_BUS_TYPE;

		EAST_BUS_OUT             : out NOC_BUS_TYPE;
		WEST_BUS_OUT             : out NOC_BUS_TYPE;
		HOR_BUS_LEFT_IN          : in  NOC_BUS_TYPE;
		HOR_BUS_RIGHT_IN         : in  NOC_BUS_TYPE;

		VER_BUS_TOP_IN           : in  NOC_BUS_TYPE;
		VER_BUS_BOTTOM_IN        : in  NOC_BUS_TYPE;
		----------------------------
		--	Partition setup I/0  
		----------------------------
		top_instruction          : out PARTITION_INSTRUCTION_RECORD_TYPE;
		bottom_instruction       : out PARTITION_INSTRUCTION_RECORD_TYPE;
		left_instruction         : out PARTITION_INSTRUCTION_RECORD_TYPE;
		right_instruction        : out PARTITION_INSTRUCTION_RECORD_TYPE;
		----------------------------
		--	AGUs and handles input  
		----------------------------
		SRAM_AGU_instruction_r   : out sram_agu_instruction_type;
		SRAM_AGU_instruction_w   : out sram_agu_instruction_type;

		agu_en_r                 : out std_logic;
		agu_en_w                 : out std_logic;
		----------------------------
		--	CROSSBAR DIRECTION  
		----------------------------
		DIRECTION                : out CORSSBAR_INSTRUCTION_RECORD_TYPE
	);
end entity iSwitch;

architecture RTL of iSwitch is
	signal left_bus_out                  : NOC_BUS_TYPE;
	signal left_bus_ver_sel              : std_logic;
	signal left_bus_hor_sel              : std_logic;
	signal left_agu_en_r                 : std_logic;
	signal left_agu_en_w                 : std_logic;
	signal left_SRAM_AGU_instruction_r   : sram_agu_instruction_type;
	signal left_SRAM_AGU_instruction_w   : sram_agu_instruction_type;
	signal left_north_instruction_out    : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal left_SOUTH_instruction_out    : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal left_east_instruction_out     : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal left_west_instruction_out     : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal right_bus_out                 : NOC_BUS_TYPE;
	signal right_bus_ver_sel             : std_logic;
	signal right_bus_hor_sel             : std_logic;
	signal right_agu_en_r                : std_logic;
	signal right_agu_en_w                : std_logic;
	signal right_SRAM_AGU_instruction_r  : sram_agu_instruction_type;
	signal right_SRAM_AGU_instruction_w  : sram_agu_instruction_type;
	signal right_north_instruction_out   : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal right_SOUTH_instruction_out   : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal right_east_instruction_out    : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal right_west_instruction_out    : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal top_bus_out                   : NOC_BUS_TYPE;
	signal top_bus_ver_sel               : std_logic;
	signal top_bus_hor_sel               : std_logic;
	signal top_agu_en_r                  : std_logic;
	signal top_agu_en_w                  : std_logic;
	signal top_SRAM_AGU_instruction_r    : sram_agu_instruction_type;
	signal top_SRAM_AGU_instruction_w    : sram_agu_instruction_type;
	signal top_north_instruction_out     : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal top_SOUTH_instruction_out     : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal top_east_instruction_out      : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal top_west_instruction_out      : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal bottom_bus_out                : NOC_BUS_TYPE;
	signal bottom_bus_ver_sel            : std_logic;
	signal bottom_bus_hor_sel            : std_logic;
	signal bottom_agu_en_r               : std_logic;
	signal bottom_agu_en_w               : std_logic;
	signal bottom_SRAM_AGU_instruction_r : sram_agu_instruction_type;
	signal bottom_SRAM_AGU_instruction_w : sram_agu_instruction_type;
	signal bottom_north_instruction_out  : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal bottom_SOUTH_instruction_out  : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal bottom_east_instruction_out   : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal bottom_west_instruction_out   : PARTITION_INSTRUCTION_RECORD_TYPE;
	signal bottom_direction              : CORSSBAR_INSTRUCTION_RECORD_TYPE;
	signal top_direction                 : CORSSBAR_INSTRUCTION_RECORD_TYPE;
	signal right_direction               : CORSSBAR_INSTRUCTION_RECORD_TYPE;
	signal left_direction                : CORSSBAR_INSTRUCTION_RECORD_TYPE;
	
	signal  left_turn_left :std_logic;
	signal right_turn_left :std_logic;
--	signal    up_turn_left :std_logic;
--	signal  down_turn_left :std_logic;

	signal  left_turn_right :std_logic;
	signal right_turn_right :std_logic;
--	signal    up_turn_right :std_logic;
--	signal  down_turn_right :std_logic;
	signal top_turn_left : std_logic;
	signal top_turn_right : std_logic;
	signal bottom_turn_left : std_logic;
	signal bottom_turn_right : std_logic;

begin
	p_direction : process(bottom_direction, top_direction, right_direction, left_direction) is
	begin
		if bottom_direction.ENABLE = '1' then
			DIRECTION <= bottom_direction;
		elsif top_direction.ENABLE = '1' then
			DIRECTION <= top_direction;
		elsif right_direction.ENABLE = '1' then
			DIRECTION <= right_direction;
		elsif left_direction.ENABLE = '1' then
			DIRECTION <= left_direction;
		else
			DIRECTION <= IGNORE;
		end if;
	end process p_direction;

	u_left_source_decoder_n_fsm : entity work.source_decoder_n_fsm
		generic map(
			This_ROW => This_ROW,
			This_COL => This_COL,
			vertical => '0'
		)
		port map(
			clk                      => clk,
			rst_n                    => rst_n,
			bus_in                   => HOR_BUS_LEFT_IN,
			east_splitter_direction  => east_splitter_direction,
			west_splitter_direction  => west_splitter_direction,
			north_splitter_direction => north_splitter_direction,
			south_splitter_direction => south_splitter_direction,
			bus_out                  => left_bus_out,
			bus_ver_sel              => left_bus_ver_sel,
			bus_hor_sel              => left_bus_hor_sel,
			turn_left => left_turn_left,
			turn_right => left_turn_right,
			agu_en_r                 => left_agu_en_r,
			agu_en_w                 => left_agu_en_w,
			SRAM_AGU_instruction_r   => left_SRAM_AGU_instruction_r,
			SRAM_AGU_instruction_w   => left_SRAM_AGU_instruction_w,
			north_instruction_out    => left_north_instruction_out,
			SOUTH_instruction_out    => left_SOUTH_instruction_out,
			east_instruction_out     => left_east_instruction_out,
			west_instruction_out     => left_west_instruction_out,
			direction                => left_direction
		);
	u_right_source_decoder_n_fsm : entity work.source_decoder_n_fsm
		generic map(
			This_ROW => This_ROW,
			This_COL => This_COL,
			vertical => '0'
		)
		port map(
			clk                      => clk,
			rst_n                    => rst_n,
			bus_in                   => HOR_BUS_RIGHT_IN,
			east_splitter_direction  => east_splitter_direction,
			west_splitter_direction  => west_splitter_direction,
			north_splitter_direction => north_splitter_direction,
			south_splitter_direction => south_splitter_direction,
			bus_out                  => right_bus_out,
			bus_ver_sel              => right_bus_ver_sel,
			bus_hor_sel              => right_bus_hor_sel,
			turn_left =>  right_turn_left,
			turn_right => right_turn_right,
			agu_en_r                 => right_agu_en_r,
			agu_en_w                 => right_agu_en_w,
			SRAM_AGU_instruction_r   => right_SRAM_AGU_instruction_r,
			SRAM_AGU_instruction_w   => right_SRAM_AGU_instruction_w,
			north_instruction_out    => right_north_instruction_out,
			SOUTH_instruction_out    => right_SOUTH_instruction_out,
			east_instruction_out     => right_east_instruction_out,
			west_instruction_out     => right_west_instruction_out,
			direction                => right_direction
		);
	u_top_source_decoder_n_fsm : entity work.source_decoder_n_fsm
		generic map(
			This_ROW => This_ROW,
			This_COL => This_COL,
			vertical => '1'
		)
		port map(
			clk                      => clk,
			rst_n                    => rst_n,
			bus_in                   => VER_BUS_TOP_IN,
			east_splitter_direction  => east_splitter_direction,
			west_splitter_direction  => west_splitter_direction,
			north_splitter_direction => north_splitter_direction,
			south_splitter_direction => south_splitter_direction,
			bus_out                  => top_bus_out,
			bus_ver_sel              => top_bus_ver_sel,
			bus_hor_sel              => top_bus_hor_sel,
			turn_left  => top_turn_left,
			turn_right => top_turn_right,		
			agu_en_r                 => top_agu_en_r,
			agu_en_w                 => top_agu_en_w,
			SRAM_AGU_instruction_r   => top_SRAM_AGU_instruction_r,
			SRAM_AGU_instruction_w   => top_SRAM_AGU_instruction_w,
			north_instruction_out    => top_north_instruction_out,
			SOUTH_instruction_out    => top_SOUTH_instruction_out,
			east_instruction_out     => top_east_instruction_out,
			west_instruction_out     => top_west_instruction_out,
			direction                => top_direction
		);
	u_bottom_source_decoder_n_fsm : entity work.source_decoder_n_fsm
		generic map(
			This_ROW => This_ROW,
			This_COL => This_COL,
			vertical => '1'
		)
		port map(
			clk                      => clk,
			rst_n                    => rst_n,
			bus_in                   => VER_BUS_BOTTOM_IN,
			east_splitter_direction  => east_splitter_direction,
			west_splitter_direction  => west_splitter_direction,
			north_splitter_direction => north_splitter_direction,
			south_splitter_direction => south_splitter_direction,
			bus_out                  => bottom_bus_out,
			bus_ver_sel              => bottom_bus_ver_sel,
			bus_hor_sel              => bottom_bus_hor_sel,
			turn_left  => bottom_turn_left,
			turn_right => bottom_turn_right,		
			agu_en_r                 => bottom_agu_en_r,
			agu_en_w                 => bottom_agu_en_w,
			SRAM_AGU_instruction_r   => bottom_SRAM_AGU_instruction_r,
			SRAM_AGU_instruction_w   => bottom_SRAM_AGU_instruction_w,
			north_instruction_out    => bottom_north_instruction_out,
			SOUTH_instruction_out    => bottom_SOUTH_instruction_out,
			east_instruction_out     => bottom_east_instruction_out,
			west_instruction_out     => bottom_west_instruction_out,
			direction                => bottom_direction
		);

	u_selector : entity work.selector
		port map(
			  left_turn_left  =>   left_turn_left ,
			  left_turn_right =>   left_turn_right,		
			 right_turn_left  =>  right_turn_left ,
			 right_turn_right =>  right_turn_right,		
			   top_turn_left  =>    top_turn_left ,
			   top_turn_right =>    top_turn_right,		
			bottom_turn_left  => bottom_turn_left ,
			bottom_turn_right => bottom_turn_right,		
			
			top_agu_en_r                  => top_agu_en_r,
			top_agu_en_w                  => top_agu_en_w,
			top_SRAM_AGU_instruction_r    => top_SRAM_AGU_instruction_r,
			top_SRAM_AGU_instruction_w    => top_SRAM_AGU_instruction_w,
			bottom_agu_en_r               => bottom_agu_en_r,
			bottom_agu_en_w               => bottom_agu_en_w,
			bottom_SRAM_AGU_instruction_r => bottom_SRAM_AGU_instruction_r,
			bottom_SRAM_AGU_instruction_w => bottom_SRAM_AGU_instruction_w,
			left_agu_en_r                 => left_agu_en_r,
			left_agu_en_w                 => left_agu_en_w,
			left_SRAM_AGU_instruction_r   => left_SRAM_AGU_instruction_r,
			left_SRAM_AGU_instruction_w   => left_SRAM_AGU_instruction_w,
			right_agu_en_r                => right_agu_en_r,
			right_agu_en_w                => right_agu_en_w,
			right_SRAM_AGU_instruction_r  => right_SRAM_AGU_instruction_r,
			right_SRAM_AGU_instruction_w  => right_SRAM_AGU_instruction_w,
			top_north_instruction_out     => top_north_instruction_out,
			top_south_instruction_out     => top_SOUTH_instruction_out,
			top_east_instruction_out      => top_east_instruction_out,
			top_west_instruction_out      => top_west_instruction_out,
			bottom_north_instruction_out  => bottom_north_instruction_out,
			bottom_south_instruction_out  => bottom_SOUTH_instruction_out,
			bottom_east_instruction_out   => bottom_east_instruction_out,
			bottom_west_instruction_out   => bottom_west_instruction_out,
			left_north_instruction_out    => left_north_instruction_out,
			left_south_instruction_out    => left_SOUTH_instruction_out,
			left_east_instruction_out     => left_east_instruction_out,
			left_west_instruction_out     => left_west_instruction_out,
			right_north_instruction_out   => right_north_instruction_out,
			right_south_instruction_out   => right_SOUTH_instruction_out,
			right_east_instruction_out    => right_east_instruction_out,
			right_west_instruction_out    => right_west_instruction_out,
			top_bus_out                   => top_bus_out,
			bottom_bus_out                => bottom_bus_out,
			left_bus_out                  => left_bus_out,
			right_bus_out                 => right_bus_out,
			top_bus_ver_sel               => top_bus_ver_sel,
			top_bus_hor_sel               => top_bus_hor_sel,
			bottom_bus_ver_sel            => bottom_bus_ver_sel,
			bottom_bus_hor_sel            => bottom_bus_hor_sel,
			left_bus_ver_sel              => left_bus_ver_sel,
			left_bus_hor_sel              => left_bus_hor_sel,
			right_bus_ver_sel             => right_bus_ver_sel,
			right_bus_hor_sel             => right_bus_hor_sel,
			north_bus_out                 => NORTH_BUS_OUT,
			south_bus_out                 => SOUTH_BUS_OUT,
			east_bus_out                  => EAST_BUS_OUT,
			west_bus_out                  => WEST_BUS_OUT,
			top_instruction               => top_instruction,
			bottom_instruction            => bottom_instruction,
			left_instruction              => left_instruction,
			right_instruction             => right_instruction,
			SRAM_AGU_instruction_r        => SRAM_AGU_instruction_r,
			SRAM_AGU_instruction_w        => SRAM_AGU_instruction_w,
			agu_en_r                      => agu_en_r,
			agu_en_w                      => agu_en_w
		);

end architecture RTL;
