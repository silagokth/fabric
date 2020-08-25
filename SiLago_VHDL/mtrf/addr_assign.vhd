-------------------------------------------------------
--! @file addr_assign.vhd
--! @brief This module is used for address assignment in each block in a fabric
--! @details 
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
-- Title      : 
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : addr_assign.vhd
-- Author     : Dimitrios Stathis
-- Company    : KTH
-- Created    : 15/01/2018
-- Last update: 2020-01-24
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2018
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 15/01/2018  1.0      Dimitrios Stathis      Created
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

--! Standard ieee library
LIBRARY ieee;
--! Standard logic library
USE ieee.std_logic_1164.ALL;
--! Standard numeric library for signed and unsigned
USE ieee.numeric_std.ALL;
USE work.noc_types_n_constants.ALL;

--! This module is used for address assignment in each block(cell) in a fabric

--! We use this module to generate and store the global address of the
--! SiLago or DiMArch cell. The module assigns one address (row and column) to
--! the block according to the address of the previous cell. The assignment can be
--! done only once after the reset of the system, and it is blocked after the first
--! assignment. We use the ROW_WIDTH and COL_WIDTH generic to set the required bits for
--! addressing.
ENTITY addr_assign IS
    PORT
    (
        clk       : IN std_logic;                         --! Clock 
        rst_n     : IN std_logic;                         --! Negative reset
        start_row : IN std_logic;                         --! Start signal (connected to the valid signal of the previous block in the same row)
        start_col : IN std_logic;                         --! Start signal (connected to the valid signal of the previous block in the same col)
        prevRow   : IN UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);  --! Row address assigned to the previous cell
        prevCol   : IN UNSIGNED(COL_WIDTH - 1 DOWNTO 0);  --! Col address assigned to the previous cell
        valid     : OUT std_logic;                        --! Valid signals, used to signal that the assignment of the address is complete
        thisRow   : OUT UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! The row address assigned to the cell
        thisCol   : OUT UNSIGNED(COL_WIDTH - 1 DOWNTO 0)  --! The column address assigned to the cell
    );
END ENTITY addr_assign;

--! @brief Simple structural architecture for address assignment.
--! @details We use a row and column signal to store the local assigned address.
--! The local address is assigned using the address of the previous cell. When 
--! the previous cell get its address assigned, it rise the valid signal. When
--! this cell reads the assertion of this signal and depending, if the signal
--! arrives from the cell in the previous row then the row address is assigned
--! by adding one (1) in the row address and keeping the column address as is. 
--! When the signal arrives from the cell in the same row, the generated address
--! is assigned by adding one in the row column address and keeps the row address as is.
ARCHITECTURE RTL OF addr_assign IS
    SIGNAL Row  : UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! Local row address 
    SIGNAL Col  : UNSIGNED(COL_WIDTH - 1 DOWNTO 0); --! Local column address 
    SIGNAL lock : std_logic;                        --! Lock signal, that signals the assignment of address
BEGIN
    thisRow <= Row;
    thisCol <= Col;
    valid   <= lock;

    address_assignment : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            Row  <= (OTHERS => '0');
            Col  <= (OTHERS => '0');
            lock <= '0';
        ELSIF rising_edge(clk) THEN
            IF (lock = '0') THEN
                IF (start_col = '1') THEN --! Signal coming from the same column (we have a change in rows)
                    row  <= prevRow + 1;
                    col  <= prevCol;
                    lock <= '1';
                END IF;
                IF (start_row = '1') THEN --! Signal coming from the same row (we have a change in column)
                    col  <= prevCol + 1;
                    row  <= prevRow;
                    lock <= '1';
                END IF;
            END IF;
        END IF;
    END PROCESS address_assignment;

END ARCHITECTURE RTL;

ARCHITECTURE RTL_first_cell OF addr_assign IS
    SIGNAL Row         : UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! Local row address 
    SIGNAL Col         : UNSIGNED(COL_WIDTH - 1 DOWNTO 0); --! Local column address 
    SIGNAL lock, start : std_logic;                        --! Lock signal, that signals the assignment of address

BEGIN

    thisRow <= Row;
    thisCol <= Col;
    valid   <= lock;
    start   <= start_row;

    addres_assignment : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            Row  <= (OTHERS => '0');
            Col  <= (OTHERS => '0');
            lock <= '0';
        ELSIF rising_edge(clk) THEN
            IF (lock = '0') THEN
                IF (start = '1') THEN --! Signal coming from the same column (we have a change in rows)
                    Row  <= (OTHERS => '0');
                    Col  <= (OTHERS => '0');
                    lock <= '1';
                END IF;
            END IF;
        END IF;
    END PROCESS addres_assignment;

END ARCHITECTURE RTL_first_cell;