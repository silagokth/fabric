-------------------------------------------------------
--! @file selector.vhd
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
-- File       : selector.vhd
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
USE ieee.NUMERIC_STD.ALL;
USE work.noc_types_n_constants.NOC_BUS_TYPE;
USE work.noc_types_n_constants.PARTITION_INSTRUCTION_RECORD_TYPE;
USE work.noc_types_n_constants.IDLE_BUS;
USE work.noc_types_n_constants.IDLE_PAR_INST;
USE work.noc_types_n_constants.sram_agu_instruction_type;
USE work.noc_types_n_constants.sram_instr_zero;
-----------------------------------------------------------
-- this code should be split based on outputs so that during layout relevant outputs 
-- can be placed closed the their edges . -- Adeel 

ENTITY selector IS
    PORT
    (
        left_turn_left, left_turn_right, right_turn_left, right_turn_right, top_turn_left, top_turn_right, bottom_turn_left, bottom_turn_right : IN std_logic;

        top_agu_en_r, top_agu_en_w       : IN std_logic;
        top_SRAM_AGU_instruction_r       : IN sram_agu_instruction_type;
        top_SRAM_AGU_instruction_w       : IN sram_agu_instruction_type;
        bottom_agu_en_r, bottom_agu_en_w : IN std_logic;
        bottom_SRAM_AGU_instruction_r    : IN sram_agu_instruction_type;
        bottom_SRAM_AGU_instruction_w    : IN sram_agu_instruction_type;
        left_agu_en_r, left_agu_en_w     : IN std_logic;
        left_SRAM_AGU_instruction_r      : IN sram_agu_instruction_type;
        left_SRAM_AGU_instruction_w      : IN sram_agu_instruction_type;
        right_agu_en_r, right_agu_en_w   : IN std_logic;
        right_SRAM_AGU_instruction_r     : IN sram_agu_instruction_type;
        right_SRAM_AGU_instruction_w     : IN sram_agu_instruction_type;

        top_north_instruction_out, top_south_instruction_out, top_east_instruction_out, top_west_instruction_out             : IN PARTITION_INSTRUCTION_RECORD_TYPE;
        bottom_north_instruction_out, bottom_south_instruction_out, bottom_east_instruction_out, bottom_west_instruction_out : IN PARTITION_INSTRUCTION_RECORD_TYPE;
        left_north_instruction_out, left_south_instruction_out, left_east_instruction_out, left_west_instruction_out         : IN PARTITION_INSTRUCTION_RECORD_TYPE;
        right_north_instruction_out, right_south_instruction_out, right_east_instruction_out, right_west_instruction_out     : IN PARTITION_INSTRUCTION_RECORD_TYPE;
        --------------------------------------------------------
        --	source decoder n fsm suggested bus out values
        --------------------------------------------------------

        top_bus_out    : IN NOC_BUS_TYPE;
        bottom_bus_out : IN NOC_BUS_TYPE;
        left_bus_out   : IN NOC_BUS_TYPE;
        right_bus_out  : IN NOC_BUS_TYPE;
        -----------------------------------------------------------------------------
        --	source decoder n fsm suggested hor_sel and vertical select values 
        -----------------------------------------------------------------------------

        top_bus_ver_sel, top_bus_hor_sel       : IN std_logic;
        bottom_bus_ver_sel, bottom_bus_hor_sel : IN std_logic;
        left_bus_ver_sel, left_bus_hor_sel     : IN std_logic;
        right_bus_ver_sel, right_bus_hor_sel   : IN std_logic;

        ----------------------------
        --	iNoC segmented bus output  
        ----------------------------

        north_bus_out : OUT NOC_BUS_TYPE;
        south_bus_out : OUT NOC_BUS_TYPE;
        east_bus_out  : OUT NOC_BUS_TYPE;
        west_bus_out  : OUT NOC_BUS_TYPE;

        ----------------------------
        --	Partition setup I/0  
        ----------------------------
        top_instruction    : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        bottom_instruction : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        left_instruction   : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        right_instruction  : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        ----------------------------
        --	AGUs and handles input  
        ----------------------------
        SRAM_AGU_instruction_r : OUT sram_agu_instruction_type;
        SRAM_AGU_instruction_w : OUT sram_agu_instruction_type;
        agu_en_r               : OUT std_logic;
        agu_en_w               : OUT std_logic
    );
END ENTITY selector;

ARCHITECTURE RTL OF selector IS
BEGIN
    -- why does this process produce multiple drivers i.e. X when all output is changed to 1
    p_top_instruction : PROCESS (top_north_instruction_out, bottom_north_instruction_out, left_north_instruction_out, right_north_instruction_out) IS
    BEGIN
        IF top_north_instruction_out.ENABLE = '1' THEN
            top_instruction <= top_north_instruction_out;
        ELSIF left_north_instruction_out.ENABLE = '1' THEN
            top_instruction <= left_north_instruction_out;
        ELSIF right_north_instruction_out.ENABLE = '1' THEN
            top_instruction <= right_north_instruction_out;
        ELSIF bottom_north_instruction_out.ENABLE = '1' THEN
            top_instruction <= bottom_north_instruction_out;
        ELSE
            top_instruction <= IDLE_PAR_INST;
        END IF;
    END PROCESS p_top_instruction;

    p_bottom_instruction : PROCESS (top_south_instruction_out, bottom_south_instruction_out, left_south_instruction_out, right_south_instruction_out) IS
    BEGIN
        IF top_south_instruction_out.ENABLE = '1' THEN
            bottom_instruction <= top_south_instruction_out;
        ELSIF left_south_instruction_out.ENABLE = '1' THEN
            bottom_instruction <= left_south_instruction_out;
        ELSIF right_south_instruction_out.ENABLE = '1' THEN
            bottom_instruction <= right_south_instruction_out;
        ELSIF bottom_south_instruction_out.ENABLE = '1' THEN
            bottom_instruction <= bottom_south_instruction_out;
        ELSE
            bottom_instruction <= IDLE_PAR_INST;
        END IF;
    END PROCESS p_bottom_instruction;

    p_left_instruction : PROCESS (top_west_instruction_out, bottom_west_instruction_out, left_west_instruction_out, right_west_instruction_out) IS
    BEGIN
        IF top_west_instruction_out.ENABLE = '1' THEN
            left_instruction <= top_west_instruction_out;
        ELSIF left_west_instruction_out.ENABLE = '1' THEN
            left_instruction <= left_west_instruction_out;
        ELSIF right_west_instruction_out.ENABLE = '1' THEN
            left_instruction <= right_west_instruction_out;
        ELSIF bottom_west_instruction_out.ENABLE = '1' THEN
            left_instruction <= bottom_west_instruction_out;
        ELSE
            left_instruction <= IDLE_PAR_INST;
        END IF;
    END PROCESS p_left_instruction;

    p_right_instruction : PROCESS (top_east_instruction_out, bottom_east_instruction_out, left_east_instruction_out, right_east_instruction_out) IS
    BEGIN
        IF top_east_instruction_out.ENABLE = '1' THEN
            right_instruction <= top_east_instruction_out;
        ELSIF left_east_instruction_out.ENABLE = '1' THEN
            right_instruction <= left_east_instruction_out;
        ELSIF right_east_instruction_out.ENABLE = '1' THEN
            right_instruction <= right_east_instruction_out;
        ELSIF bottom_east_instruction_out.ENABLE = '1' THEN
            right_instruction <= bottom_east_instruction_out;
        ELSE
            right_instruction <= IDLE_PAR_INST;
        END IF;
    END PROCESS p_right_instruction;

    p_agu_r : PROCESS (top_agu_en_r, bottom_agu_en_r, left_agu_en_r, right_agu_en_r, top_SRAM_AGU_instruction_r, bottom_SRAM_AGU_instruction_r, left_SRAM_AGU_instruction_r, right_SRAM_AGU_instruction_r) IS
    BEGIN
        IF bottom_agu_en_r = '1' THEN
            SRAM_AGU_instruction_r <= bottom_SRAM_AGU_instruction_r;
            agu_en_r               <= bottom_agu_en_r;
        ELSIF left_agu_en_r = '1' THEN
            SRAM_AGU_instruction_r <= left_SRAM_AGU_instruction_r;
            agu_en_r               <= left_agu_en_r;
        ELSIF right_agu_en_r = '1' THEN
            SRAM_AGU_instruction_r <= right_SRAM_AGU_instruction_r;
            agu_en_r               <= right_agu_en_r;
        ELSIF top_agu_en_r = '1' THEN
            SRAM_AGU_instruction_r <= top_SRAM_AGU_instruction_r;
            agu_en_r               <= top_agu_en_r;
        ELSE
            SRAM_AGU_instruction_r <= sram_instr_zero;--(others => '0');
            agu_en_r               <= '0';
        END IF;

    END PROCESS p_agu_r;
    p_agu_w : PROCESS (top_agu_en_w, bottom_agu_en_w, left_agu_en_w, right_agu_en_w, top_SRAM_AGU_instruction_w, bottom_SRAM_AGU_instruction_w, left_SRAM_AGU_instruction_w, right_SRAM_AGU_instruction_w) IS
    BEGIN
        IF bottom_agu_en_w = '1' THEN
            SRAM_AGU_instruction_w <= bottom_SRAM_AGU_instruction_w;
            agu_en_w               <= bottom_agu_en_w;
        ELSIF left_agu_en_w = '1' THEN
            SRAM_AGU_instruction_w <= left_SRAM_AGU_instruction_w;
            agu_en_w               <= left_agu_en_w;
        ELSIF right_agu_en_w = '1' THEN
            SRAM_AGU_instruction_w <= right_SRAM_AGU_instruction_w;
            agu_en_w               <= right_agu_en_w;
        ELSIF top_agu_en_w = '1' THEN
            SRAM_AGU_instruction_w <= top_SRAM_AGU_instruction_w;
            agu_en_w               <= top_agu_en_w;
        ELSE
            SRAM_AGU_instruction_w <= sram_instr_zero;--(others => '0');
            agu_en_w               <= '0';
        END IF;

    END PROCESS p_agu_w;

    p_north_bus_out : PROCESS (top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_ver_sel, bottom_bus_ver_sel, left_bus_ver_sel, right_bus_ver_sel) IS
    BEGIN
        IF left_bus_ver_sel = '1' THEN
            north_bus_out <= left_bus_out;
        ELSIF right_bus_ver_sel = '1' THEN
            north_bus_out <= right_bus_out;
        ELSIF bottom_bus_ver_sel = '1' THEN
            north_bus_out <= bottom_bus_out;
        ELSIF top_bus_ver_sel = '1' THEN
            north_bus_out <= top_bus_out;
        ELSE
            north_bus_out <= IDLE_BUS;
        END IF;
    END PROCESS p_north_bus_out;

    p_south_bus_out : PROCESS (top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_ver_sel, bottom_bus_ver_sel, left_bus_ver_sel, right_bus_ver_sel) IS
    BEGIN
        IF left_bus_ver_sel = '1' THEN
            south_bus_out <= left_bus_out;
        ELSIF right_bus_ver_sel = '1' THEN
            south_bus_out <= right_bus_out;
        ELSIF bottom_bus_ver_sel = '1' THEN
            south_bus_out <= bottom_bus_out;
        ELSIF top_bus_ver_sel = '1' THEN
            south_bus_out <= top_bus_out;
        ELSE
            south_bus_out <= IDLE_BUS;
        END IF;
    END PROCESS p_south_bus_out;

    p_east_bus_out : PROCESS (top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_hor_sel, bottom_bus_hor_sel, left_bus_hor_sel, right_bus_hor_sel, bottom_turn_right) IS
    BEGIN
        IF left_bus_hor_sel = '1' THEN
            east_bus_out <= left_bus_out;
        ELSIF right_bus_hor_sel = '1' THEN
            east_bus_out <= right_bus_out;
        ELSIF bottom_bus_hor_sel = '1' AND bottom_turn_right = '1' THEN
            east_bus_out <= bottom_bus_out;
        ELSIF top_bus_hor_sel = '1' THEN
            east_bus_out <= top_bus_out;
        ELSE
            east_bus_out <= IDLE_BUS;
        END IF;
    END PROCESS p_east_bus_out;

    p_west_bus_out : PROCESS (top_bus_out, bottom_bus_out, left_bus_out, right_bus_out, top_bus_hor_sel, bottom_bus_hor_sel, left_bus_hor_sel, right_bus_hor_sel, bottom_turn_left) IS
    BEGIN
        IF left_bus_hor_sel = '1' THEN
            west_bus_out <= left_bus_out;
        ELSIF right_bus_hor_sel = '1' THEN
            west_bus_out <= right_bus_out;
        ELSIF bottom_bus_hor_sel = '1' AND bottom_turn_left = '1' THEN
            west_bus_out <= bottom_bus_out;
        ELSIF top_bus_hor_sel = '1' THEN
            west_bus_out <= top_bus_out;
        ELSE
            west_bus_out <= IDLE_BUS;
        END IF;
    END PROCESS p_west_bus_out;

END ARCHITECTURE RTL;