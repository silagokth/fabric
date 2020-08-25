-------------------------------------------------------
--! @file source_decoder_n_fsm.vhd
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
-- File       : source_decoder_n_fsm.vhd
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

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
--use work.top_consts_types_package.SRAM_AGU_INSTR_WIDTH;
USE work.crossbar_types_n_constants.CORSSBAR_INSTRUCTION_RECORD_TYPE;

USE work.noc_types_n_constants.ALL;

ENTITY source_decoder_n_fsm IS
    GENERIC
    (
        vertical : std_logic := '1');

    PORT
    (
        clk    : IN std_logic;
        rst_n  : IN std_logic;
        bus_in : IN NOC_BUS_TYPE;

        This_ROW : IN UNSIGNED (ROW_WIDTH - 1 DOWNTO 0);
        This_COL : IN UNSIGNED (COL_WIDTH - 1 DOWNTO 0);

        east_splitter_direction, west_splitter_direction, north_splitter_direction, south_splitter_direction : IN std_logic_vector (1 DOWNTO 0);

        bus_out                                                                                  : OUT NOC_BUS_TYPE;
        bus_ver_sel, bus_hor_sel                                                                 : OUT std_logic;
        turn_left, turn_right                                                                    : OUT std_logic;
        agu_en_r, agu_en_w                                                                       : OUT std_logic;
        SRAM_AGU_instruction_r                                                                   : OUT sram_agu_instruction_type;
        SRAM_AGU_instruction_w                                                                   : OUT sram_agu_instruction_type;
        north_instruction_out, SOUTH_instruction_out, east_instruction_out, west_instruction_out : OUT PARTITION_INSTRUCTION_RECORD_TYPE;

        direction : OUT CORSSBAR_INSTRUCTION_RECORD_TYPE
    );
END ENTITY source_decoder_n_fsm;

ARCHITECTURE RTL OF source_decoder_n_fsm IS
    SIGNAL RETRANSMIT_FLAG                                                          : STD_LOGIC;
    SIGNAL SEGMENT_SRC_FLAG                                                         : STD_LOGIC;
    SIGNAL SEGMENT_DST_FLAG                                                         : STD_LOGIC;
    SIGNAL flip_transmit                                                            : STD_LOGIC;
    SIGNAL north_instruction, SOUTH_instruction, east_instruction, west_instruction : PARTITION_INSTRUCTION_RECORD_TYPE;
    --	signal turn_left : std_logic;
    --	signal turn_right : std_logic;
BEGIN
    u_location_decoder : ENTITY work.source_decoder(RTL)
        GENERIC
        MAP (
        VERTICAL => vertical
        )
        PORT MAP
        (

            --		rst_n         => rst_n,
            --		clk           => clk,
            --		rw => rw,
            This_ROW          => std_logic_vector(This_ROW),
            This_COL          => std_logic_vector(This_COL),
            NOC_BUS_IN        => bus_in,
            DIRECTION_OUT     => direction,
            RETRANSMIT        => RETRANSMIT_FLAG,
            SEGMENT_SRC       => SEGMENT_SRC_FLAG,
            SEGMENT_DST       => SEGMENT_DST_FLAG,
            flip_transmit     => flip_transmit,
            NORTH_instruction => north_instruction,
            SOUTH_instruction => SOUTH_instruction,
            EAST_instruction  => east_instruction,
            WEST_instruction  => west_instruction
        );

    u_source_fsm : ENTITY work.source_fsm(RTL)
        GENERIC
        MAP (vertical => '1')
        PORT
        MAP (
        rst_n                    => rst_n,
        clk                      => clk,
        bus_in                   => bus_in,
        RETRANSMIT               => RETRANSMIT_FLAG,
        SEGMENT_SRC              => SEGMENT_SRC_FLAG,
        SEGMENT_DST              => SEGMENT_DST_FLAG,
        flip_transmit            => flip_transmit,
        NORTH_instruction        => north_instruction,
        SOUTH_instruction        => SOUTH_instruction,
        EAST_instruction         => east_instruction,
        WEST_instruction         => west_instruction,
        east_splitter_direction  => east_splitter_direction,
        west_splitter_direction  => west_splitter_direction,
        north_splitter_direction => north_splitter_direction,
        south_splitter_direction => south_splitter_direction,
        bus_out                  => bus_out,
        bus_ver_sel              => bus_ver_sel,
        bus_hor_sel              => bus_hor_sel,
        turn_left                => turn_left,
        turn_right               => turn_right,
        agu_en_r                 => agu_en_r,
        agu_en_w                 => agu_en_w,
        SRAM_AGU_instruction_r   => SRAM_AGU_instruction_r,
        SRAM_AGU_instruction_w   => SRAM_AGU_instruction_w
        );

    north_instruction_out <= north_instruction;
    SOUTH_instruction_out <= SOUTH_instruction;
    east_instruction_out  <= east_instruction;
    west_instruction_out  <= west_instruction;
END ARCHITECTURE RTL;