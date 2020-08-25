-------------------------------------------------------
--! @file
--! @brief SRAM Tile
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
-- Title      : SRAM
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : SRAM.vhd
-- Author     : Muhammad Ali Shami <shami@kth.se>
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
--             1.0      Muhammad Ali Shami        Created
--             1.5                                Segmented bus is replaced with buffers
-- 2019-03-11  2.0      Dimitrios Stathis         Update SRAM module into async and removed extra signals
-- 2019-10-07  3.0      Dimitrios Stathis         Convereted to a wrapper for the actual model
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

--! IEEE library
LIBRARY IEEE;
--! Use standard library
USE IEEE.std_logic_1164.ALL;
--! Use numeric library for arithmetic operations
USE ieee.numeric_std.ALL;
--! Include the sram address width
USE work.top_consts_types_package.SRAM_ADDRESS_WIDTH;
--! Include sram depth
USE work.top_consts_types_package.SRAM_DEPTH;
--! Include the sram width
USE work.top_consts_types_package.SRAM_WIDTH;
--! Use the noc package, includes all the type and constant definition 
USE work.noc_types_n_constants.ALL;
--! Use the misc package
USE work.misc.ALL;
--! Include the number of dimarch columns
USE work.top_consts_types_package.COLUMNS;
--! Include the bidwidt from the top package
USE work.top_consts_types_package.BITWIDTH;
--! Include the textio package for read and write files (not in use)
USE STD.TEXTIO.ALL;

--! @brief  This is a wrapper for the SRAM macros
--! @detail We use 2 SRAM macros of 128bit data bus and 64 words
ENTITY SRAM IS
    PORT
    (
        CLK      : IN std_logic;                                         --! Clock 
        ADDR_W   : IN std_logic_vector(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); --! Write Address
        WEB      : IN std_logic;                                         --! Write enable low 
        DATA_IN  : IN std_logic_vector(SRAM_WIDTH - 1 DOWNTO 0);         --! Data in to the SRAM
        ADDR_R   : IN std_logic_vector(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); --! Read Address
        REB      : IN std_logic;                                         --! Read  enable low
        DATA_OUT : OUT std_logic_vector(SRAM_WIDTH - 1 DOWNTO 0)         --! Data out to the SRAM
    );
END SRAM;

-- TODO : We might need to create a simple simulation model for the SRAM (same name as the actual and use that one instead of the "netlist" of the SRAM),OR model the setup and hold times

--! @brief Architecture of the SRAM
--! @detail This a wrapper for the SRAM simulation and synthesis, it uses an SRAM macro.
--! For the Simulation a set of constants is used to determinate the set-up and hold timings of the signals.
--! The setup value is used to delay the clock so that all signals arrive on time. The different hold
--! times are used to delay the change of the appropriate signals.
ARCHITECTURE SRAM_behaviour OF SRAM IS

    COMPONENT SRAM_model IS
        PORT
        (
            SLP   : IN std_logic;                      --! Sleep mode, active high - asynch
            SD    : IN std_logic;                      --! Shut down mode, active high - asynch
            CLK   : IN std_logic;                      --! Clock 
            CEB   : IN std_logic;                      --! Chip enable, active low
            WEB   : IN std_logic;                      --! Write enable, active low
            CEBM  : IN std_logic;                      --! Chip enable for BIST mode, active low
            WEBM  : IN std_logic;                      --! Write enable for BIST, active low
            AWT   : IN std_logic;                      --! Asynch write through
            A     : IN std_logic_vector(5 DOWNTO 0);   --! Address input
            D     : IN std_logic_vector(127 DOWNTO 0); --! Data input
            BWEB  : IN std_logic_vector(127 DOWNTO 0); --! Bit write enable, active low
            AM    : IN std_logic_vector(5 DOWNTO 0);   --! Address input for BIST mode
            DM    : IN std_logic_vector(127 DOWNTO 0); --! Data input for BIST mode
            BWEBM : IN std_logic_vector(127 DOWNTO 0); --! Bit write enable for BIST mode, active low
            BIST  : IN std_logic;                      --! BIST interface enable, active high
            Q     : OUT std_logic_vector(127 DOWNTO 0) --! Data out
        );
    END COMPONENT SRAM_model;
    SIGNAL ADDR  : std_logic_vector(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); --! Address to the sram blocks
    SIGNAL CEB   : std_logic;                                         --! Chip enable signal
    SIGNAL AWT   : std_logic;                                         --! Concurrent read and write
    SIGNAL WEB_S : std_logic;                                         --! Write enable sram
    SIGNAL SLP   : std_logic;                                         --! TODO : Add the sleep function and disable sram when not in use
    SIGNAL SD    : std_logic;                                         --! TODO : Add the shutdown function and power down sram when not in use

    -- Delayed signals
    SIGNAL CLK_delay     : std_logic;                                         --! Clock delayed to match the setup time
    SIGNAL CEB_delay     : std_logic;                                         --! Chip enabled delayed to match the hold time
    SIGNAL ADDR_delay    : std_logic_vector(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); --! Address delayed to match the hold time
    SIGNAL DATA_IN_delay : std_logic_vector(SRAM_WIDTH - 1 DOWNTO 0);         --! Data in delayed to match the hold time
    SIGNAL WEB_delay     : std_logic;                                         --! Write enable delayed to match the hold time

BEGIN
    SLP <= '0'; --! TODO Need to be delayed approprietly (Sleep mode)
    SD  <= '0'; --! TODO Need to be delayed approprietly (Shut Down mode)
    -- Singal pre-processing for SRAM
    logic_proc : PROCESS (REB, WEB, ADDR_R, ADDR_W, CLK)
    BEGIN
        --Default values
        ADDR  <= (OTHERS => '0');
        CEB   <= '0'; -- Enable chip
        AWT   <= '0';
        WEB_S <= '0';
        -- CE, WE and address assignment
        IF (REB = '1' AND WEB = '0') THEN -- Read enable 
            CEB   <= '0';                     -- Enable chip
            ADDR  <= ADDR_R;                  -- Use read address
            AWT   <= '0';
            WEB_S <= '1';
        ELSIF (REB = '0' AND WEB = '1') THEN -- Write in the SRAM
            CEB   <= '0';                        -- Enable chip
            ADDR  <= ADDR_W;                     -- Give the SRAM the write address
            AWT   <= '0';
            WEB_S <= '0';
        ELSIF (REB = '1' AND WEB = '1') THEN -- Write + Read enable
            -- Writing and reading in different addresses is not allowed in single port SRAMS
            ASSERT (ADDR_R = ADDR_W)
            REPORT "Read and write in different addresses is not allowed"
                SEVERITY ERROR;
            CEB   <= '0';    -- Enable chip select
            ADDR  <= ADDR_W; -- Pass the address (read and write address should be the same)
            AWT   <= '1';    -- Enable the concurent AWT
            WEB_S <= '0';
        ELSE          -- No read/write
            CEB   <= '1'; -- Disable chip select, go to standby
            AWT   <= '0'; -- Disable concurrent AWT
            WEB_S <= '1';
            ADDR  <= (OTHERS => '0');
        END IF;
    END PROCESS; --logic proc

    -- All the signlas are synchronous in the RTL so we just need to delay them for the appropriate time
    CLK_delay     <= CLK;
    CEB_delay     <= CEB;
    ADDR_delay    <= ADDR;
    DATA_IN_delay <= DATA_IN;
    WEB_delay     <= WEB_S;

    sram_blocks : FOR i IN 0 TO 1 GENERATE
        u_sram_block_X : SRAM_model
        PORT MAP
        (
            SLP  => SLP,
            SD   => SD,
            CLK  => CLK_delay,
            CEB  => CEB_delay,
            WEB  => WEB_delay,
            CEBM => '1',
            WEBM => '1',
            AWT  => AWT,
            A    => ADDR_delay(5 DOWNTO 0),
            D    => DATA_IN_delay((127 + 128 * i) DOWNTO (128 * i)),
            BWEB => (OTHERS => '0'),
            AM => (OTHERS => '0'),
            DM => (OTHERS => '0'),
            BWEBM => (OTHERS => '0'),
            BIST => '0',
            Q    => DATA_OUT((127 + 128 * i) DOWNTO (128 * i))
        );
    END GENERATE sram_blocks; -- sram blocks

END SRAM_behaviour;