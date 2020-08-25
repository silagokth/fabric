-------------------------------------------------------
--! @file SRAM_model.vhd
--! @brief SRAM abstracct model
--! @details Only to be used for simulation !!!!
--! @author Dimitrios Stathis
--! @version 1.0
--! @date 2020-01-24
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
-- Title      : SRAM abstract model
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : SRAM_model.vhd
-- Author     : Dimitrios Stathis
-- Company    : KTH
-- Created    : 2020-01-24
-- Last update: 2020-01-24
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2020
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2020-01-24  1.0      Dimitrios Stathis      Created
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

--! IEEE Library and work
LIBRARY IEEE, work;
--! Use standard library
USE IEEE.std_logic_1164.ALL;
--! Use numeric standard library for arithmetic operations
USE ieee.numeric_std.ALL;
--! Use top_consts_types_package package
USE work.top_consts_types_package.ALL;

--! Basic Model of a synchronous SRAM

--! This a simple SRAM model, only used for simulations. 
--! The SRAM models the behavior of a 128 bit-width SRAM with Chip Enable,
--! Write enable, sleep mode, shutdown mode and write through. 
ENTITY SRAM_model IS
    PORT
    (
        SLP   : IN std_logic;                      --! Sleep mode, active high - asynch
        SD    : IN std_logic;                      --! Shut down mode, active high - asynch
        CLK   : IN std_logic;                      --! Clock 
        CEB   : IN std_logic;                      --! Chip enable, active low
        WEB   : IN std_logic;                      --! Write enable, active low
        CEBM  : IN std_logic;                      --! Chip enable for BIST mode, active low !!!Not Modeled!!! used here to be compatible with actual SRAM
        WEBM  : IN std_logic;                      --! Write enable for BIST, active low !!!Not Modeled!!! used here to be compatible with actual SRAM
        AWT   : IN std_logic;                      --! Asynch write through
        A     : IN std_logic_vector(5 DOWNTO 0);   --! Address input
        D     : IN std_logic_vector(127 DOWNTO 0); --! Data input
        BWEB  : IN std_logic_vector(127 DOWNTO 0); --! Bit write enable, active low, used together with AWT only
        AM    : IN std_logic_vector(5 DOWNTO 0);   --! Address input for BIST mode !!!Not Modeled!!! used here to be compatible with actual SRAM
        DM    : IN std_logic_vector(127 DOWNTO 0); --! Data input for BIST mode !!!Not Modeled!!! used here to be compatible with actual SRAM
        BWEBM : IN std_logic_vector(127 DOWNTO 0); --! Bit write enable for BIST mode, active low!!!Not Modeled!!! used here to be compatible with actual SRAM
        BIST  : IN std_logic;                      --! BIST interface enable, active high
        Q     : OUT std_logic_vector(127 DOWNTO 0) --! Data out
    );
END SRAM_model;
--! @brief Behaviour of the SRAM
--! @details A simple model that simulates the behavior of an SRAM
ARCHITECTURE RTL OF SRAM_model IS
    TYPE memory_ty IS ARRAY (NATURAL RANGE <>) OF std_logic_vector(127 DOWNTO 0);
    shared VARIABLE memory : memory_ty(SRAM_DEPTH - 1 DOWNTO 0) := (OTHERS => (OTHERS => '0'));
    SIGNAL OLD      : std_logic_vector(127 DOWNTO 0);
BEGIN
    -- SLP  No read no write keep data
    -- SD   Shut down (loose data)
    -- CEB  Chip enable
    -- WEB  Write enable
    -- AWT  Write throuugh 
    -- A    Address 
    -- D    Data in 
    -- Q    Data out
    Memory_write : PROCESS (clk, CEB, SD, SLP)
    BEGIN
        IF (SD = '1') THEN
            memory := (OTHERS => (OTHERS => 'X'));
            OLD <= (OTHERS => '0');
        ELSIF (CEB = '0') AND (SLP = '0') THEN -- If not sleep mode and chip-enabeled then disable the in/out
            IF rising_edge(clk) THEN
                IF (WEB = '0') THEN
                    memory(to_integer(unsigned(A))) := D;
                ELSE
                    OLD <= memory(to_integer(unsigned(A)));
                END IF;
            END IF;
        END IF;

    END PROCESS Memory_write;
    Memory_read : PROCESS (clk, CEB, SD, SLP, WEB)
    BEGIN
        IF (SD = '1') THEN
            Q <= (OTHERS => '0');
        ELSIF (CEB = '0') AND (SLP = '0') THEN -- If not sleep mode and chip-enabeled then disable the in/out
            IF (WEB = '1') THEN
                Q <= memory(to_integer(unsigned(A)));
            ELSE
                IF (AWT = '1') THEN
                    Q <= memory(to_integer(unsigned(A)));
                ELSE
                    Q <= BWEB XOR OLD;
                END IF;
            END IF;
        ELSIF (CEB = '1') THEN --! Chip not enabled
            Q <= OLD;
        ELSIF (SLP = '1') THEN
            Q <= (OTHERS => '0');
        END IF;

    END PROCESS Memory_read;
END RTL;