-------------------------------------------------------
--! @file DiMArchTile_Bot_left_corner.vhd
--! @brief This is the DiMArch bottom tile
--! @details 
--! @author Dimitrios Stathis
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
-- File       : DiMArchTile_Bot_left_corner.vhd
-- Author     : Dimitrios Stathis
-- Company    : KTH
-- Created    : 
-- Last update: 
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2019
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
--             1.0      Dimitrios Stathis      Created
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

---------------------------------------------------------------------------
-- This is the DiMArch bottom tile. This tile is the combination of all the components
-- That are needed to connect to the DRRA fabric. It is stand-alone (can be harden)
-- and has dynamic addressing
-- Includes the following:
-- Bus segment (set on the right hand side of the SRAMTile) 
-- SRAMTile 
-- bus_selector (was part of the DRRA fabric)
---------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.top_consts_types_package.ALL;
USE work.noc_types_n_constants.ALL;
USE work.crossbar_types_n_constants.CORSSBAR_INSTRUCTION_RECORD_TYPE;
USE work.crossbar_types_n_constants.nr_of_crossbar_ports;
USE work.misc.ALL;
USE work.ALL;
--! Number of bits used to address the rows
--USE work.hw_setting.ROW_WIDTH;
--! Number of bits used to address the columns 
--USE work.hw_setting.COL_WIDTH;

--! @brief This is the DiMArch bottom tile.
--! @details This tile is the combination of all the components
--! That are needed to connect to the DRRA fabric. It is stand-alone (can be harden)
--! and has dynamic addressing
--! Includes the following:
--! \verbatim
--! Address assignment unit
--! Bus segment (set on the right hand side of the SRAMTile) 
--! SRAMTile 
--! bus_selector (was part of the DRRA fabric)
--! \endverbatim
ENTITY DiMArchTile_Bot_left_corner IS
    PORT (
        rst_n : IN STD_LOGIC;
        clk   : IN STD_LOGIC;
        -------------------------
        -- Address signals
        -------------------------
        --		start_row                 : in  std_logic; --! Start signal (connected to the valid signal of the previous block in the same row)
        start_col : IN std_logic;                         --! Start signal (connected to the valid signal of the previous block in the same col)
        prevRow   : IN UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);  --! Row address assigned to the previous cell
        prevCol   : IN UNSIGNED(COL_WIDTH - 1 DOWNTO 0);  --! Col address assigned to the previous cell
        valid     : OUT std_logic;                        --! Valid signals, used to signal that the assignment of the address is complete
        thisRow   : OUT UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! The row address assigned to the cell
        thisCol   : OUT UNSIGNED(COL_WIDTH - 1 DOWNTO 0); --! The column address assigned to the cell
        -------------------------
        -- crossbar data signals
        -------------------------
        ----------------------------
        -- Top Signals not in used
        ----------------------------
        north_out : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        north_in  : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        ----------------------------
        south_in : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        east_in  : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        --west_in                   : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
        south_out : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        east_out  : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        --west_out                  : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
        --------------------------------------------------
        -- DIRECTION OF Neibouring buses
        --------------------------------------------------
        ----------------------------
        -- Top Signals not in used
        ----------------------------
        top_splitter_direction : IN std_logic_vector(1 DOWNTO 0); --! comes from the top (south splitter)
        ------------------------------		south_splitter_direction  : in  std_logic_vector(1 DOWNTO 0);
        right_splitter_direction : IN std_logic_vector(1 DOWNTO 0); --! comes from the right(west splitter)
        -- left_splitter_direction   : out std_logic_vector(1 DOWNTO 0); --! goes from the seg_bus to the left (west splitter output)
        --------------------------------------------------
        -- partitioning instruction to neibouring buses
        --------------------------------------------------
        ----------------------------
        -- Top Signals not in used
        ----------------------------
        top_instruction_out : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        ----------------------------		
        --left_instruction_in       : in  PARTITION_INSTRUCTION_RECORD_TYPE;
        --		bottom_instruction_out    : out PARTITION_INSTRUCTION_RECORD_TYPE;
        right_instruction_out : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        ----------------------------
        --	SEMENTED BUS I/0  
        ----------------------------
        --HOR_BUS_LEFT_IN           : in  NOC_BUS_TYPE;
        HOR_BUS_RIGHT_IN : IN NOC_BUS_TYPE;
        --HOR_BUS_LEFT_OUT          : out NOC_BUS_TYPE;
        HOR_BUS_RIGHT_OUT : OUT NOC_BUS_TYPE;
        VER_BUS_BOTTOM_IN : IN NOC_BUS_TYPE;
        --VER_BUS_BOTTOM_IN_0       : in  NOC_BUS_TYPE;
        --VER_BUS_BOTTOM_IN_1       : in  NOC_BUS_TYPE;
        ----------------------------
        -- Top Signals not in used
        ----------------------------
        VER_BUS_TOP_IN  : IN NOC_BUS_TYPE;
        VER_BUS_TOP_OUT : OUT NOC_BUS_TYPE;
        ----------------------------
        --	VER_BUS_BOTTOM_OUT        : out NOC_BUS_TYPE;
        --inputs
        --outputs
        --------------------------------------------------------
        --SRAM initialization from testbench -- input signals from the left hand side
        --------------------------------------------------------
        tb_en    : IN STD_LOGIC;                                         -- Write Enable from test bench
        tb_addrs : IN STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); -- Write Address from test bench
        tb_inp   : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        tb_ROW   : IN UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
        tb_COL   : IN UNSIGNED(COL_WIDTH - 1 DOWNTO 0);
        --------------------------------------------------------
        --SRAM initialization from testbench -- output signals from the right hand side
        --------------------------------------------------------
        tb_en_out    : OUT STD_LOGIC;                                         -- Write Enable from test bench
        tb_addrs_out : OUT STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); -- Write Address from test bench
        tb_inp_out   : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        tb_ROW_out   : OUT UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
        tb_COL_out   : OUT UNSIGNED(COL_WIDTH - 1 DOWNTO 0)
    ); -- Make a new package file and define a constant for this signal
END DiMArchTile_Bot_left_corner;
--! @brief Structural architecture of the tile.
--! @details The structure of the module can be seen here:
--! \image html Dimarch_bot.png "DiMArch bottom row cells"
--! Includes the following:
--! \verbatim
--! Address assignment unit
--! Bus segment (set on the right hand side of the SRAMTile) 
--! SRAMTile 
--! bus_selector (was part of the DRRA fabric)
--! input selector (selects the input of the Dimarch cell (top or bottom row of the DRRA)
--! \endverbatim
ARCHITECTURE behv_rtl OF DiMArchTile_Bot_left_corner IS
    ------------------------------------------------------
    -- Top signals because removed from input
    ------------------------------------------------------
    --SIGNAL VER_BUS_TOP_IN            : NOC_BUS_TYPE;
    --SIGNAL VER_BUS_TOP_OUT           : NOC_BUS_TYPE;
    --SIGNAL top_instruction_out       : PARTITION_INSTRUCTION_RECORD_TYPE;
    --SIGNAL north_out                 : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
    --SIGNAL north_in                  : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
    --SIGNAL top_splitter_direction    : std_logic_vector(1 DOWNTO 0); --! comes from the top (south splitter)
    ------------------------------------------------------
    -- West signals because removed from input
    ------------------------------------------------------	
    SIGNAL west_in, west_out                 : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
    SIGNAL HOR_BUS_LEFT_IN, HOR_BUS_LEFT_OUT : NOC_BUS_TYPE;
    SIGNAL left_splitter_direction           : std_logic_vector(1 DOWNTO 0);
    SIGNAL left_instruction_in               : PARTITION_INSTRUCTION_RECORD_TYPE;
    ---------
    -- Address signals
    -------------------------
    SIGNAL This_ROW : UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! The row address assigned to the cell
    SIGNAL This_COL : UNSIGNED(COL_WIDTH - 1 DOWNTO 0); --! The column address assigned to the cell
    -------------------------
    -- Segmented bus
    -------------------------
    SIGNAL bus_direction : std_logic_vector(1 DOWNTO 0);

    SIGNAL SRAM_INST_hor_left_in   : NOC_BUS_TYPE;
    SIGNAL SRAM_INST_hor_left_out  : NOC_BUS_TYPE;
    SIGNAL SRAM_INST_hor_right_in  : NOC_BUS_TYPE;
    SIGNAL SRAM_INST_hor_right_out : NOC_BUS_TYPE;

    SIGNAL PARTITION_INST_left  : PARTITION_INSTRUCTION_RECORD_TYPE;
    SIGNAL PARTITION_INST_right : PARTITION_INSTRUCTION_RECORD_TYPE;
    SIGNAL noc_bus_out          : NOC_BUS_TYPE;
    SIGNAL noc_bus_out_reg      : NOC_BUS_TYPE;
    -------------------------
    SIGNAL DATA_NORTH : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);

    SIGNAL clk_and_conf : std_logic; --! configuration of mux

BEGIN
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Register and transmit global signals
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    --TODO A new type of configuration is required, more efficient 
    register_transfer_global : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            tb_en_out    <= '0';
            tb_addrs_out <= (OTHERS => '0');
            tb_inp_out   <= (OTHERS => '0');
            tb_ROW_out   <= (OTHERS => '0');
            tb_COL_out   <= (OTHERS => '0');
            ELSIF rising_edge(clk) THEN
            tb_en_out    <= tb_en;
            tb_addrs_out <= tb_addrs;
            tb_inp_out   <= tb_inp;
            tb_ROW_out   <= tb_ROW;
            tb_COL_out   <= tb_COL;
        END IF;
    END PROCESS register_transfer_global;
    -------------------------
    -- segmented bus signals 
    -------------------------

    left_instruction_in     <= ('0', (OTHERS => '0'), '0', '0');
    left_splitter_direction <= bus_direction;
    west_in                 <= (OTHERS => '0');
    HOR_BUS_LEFT_IN         <= ('0', (OTHERS => '0'), (OTHERS => '0'));
    SRAM_INST_hor_left_in   <= HOR_BUS_LEFT_IN;
    HOR_BUS_LEFT_OUT        <= SRAM_INST_hor_left_out;

    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- This block is the input selector, it should be moved to a different file
    -- It takes input from the DRRA (data from both cells) and rd signals and selects the input of the SRAMTile
    -- Moved to silago cells so that we reduce the amount of wires
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    --DATA_NORTH <= dimarch_silego_data_out; --_0 WHEN (dimarch_silego_rd_2_out_0 = '1')
    --else dimarch_silego_data_out_1 WHEN (dimarch_silego_rd_2_out_1 = '1')
    --else zero_block;
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Register the noc input from the DRRA
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    noc_bus_out <= VER_BUS_BOTTOM_IN;

    noc_bus_reg : PROCESS (clk, rst_n)
    BEGIN
        IF (rst_n = '0') THEN
            noc_bus_out_reg <= IDLE_BUS;
            ELSIF rising_edge(clk) THEN
            noc_bus_out_reg <= noc_bus_out;
        END IF;
    END PROCESS noc_bus_reg;

    -- 	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    -- 	BUS SELECTOR
    -- 	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- 	This selector get the noc_bus_in signals from the DRRA cells (both rows, same col) and sends it to the SRAMTile
    -- 	Needs to be moved 
    -- 	u_bus_selector : entity work.bus_selector
    -- 	generic map(this_column => i)
    -- 		port map(
    -- 			noc_bus_in0 => VER_BUS_BOTTOM_IN_0,
    -- 			noc_bus_in1 => VER_BUS_BOTTOM_IN_1,
    -- 			noc_bus_out => noc_bus_out
    -- 		);
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Segmented bus
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    u_segmented_bus_hor : ENTITY work.segmented_bus
        PORT MAP(
            clk               => clk,
            rst               => rst_n,
            left_instruction  => left_instruction_in,
            right_instruction => PARTITION_INST_left,
            left_in           => SRAM_INST_hor_left_in,
            left_out          => SRAM_INST_hor_left_out, --out
            right_in          => SRAM_INST_hor_right_in,
            right_out         => SRAM_INST_hor_right_out, --out
            bus_direction     => bus_direction            --out
        );
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Address assignment unit
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    u_addres_assign : ENTITY work.addr_assign(RTL)
        PORT MAP(
            clk       => clk,
            rst_n     => rst_n,
            start_row => '0',
            start_col => start_col,
            prevRow   => prevRow,
            prevCol   => prevCol,
            valid     => valid,
            thisRow   => This_ROW,
            thisCol   => This_COL
        );
    thisRow <= This_ROW;
    thisCol <= This_COL;
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- STile
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    u_STILE : ENTITY work.STile(behv_rtl)
        GENERIC MAP(ID => 1)
        PORT MAP(
            rst_n => rst_n,
            clk   => clk,
            -- address 
            This_ROW => This_ROW,
            This_COL => This_COL,
            -- data interconnect
            north_out => north_out,
            south_in  => south_in,
            north_in  => north_in,
            south_out => south_out,
            east_in   => east_in,
            west_out  => west_out,
            east_out  => east_out,
            west_in   => west_in,
            -- direction of neibouring busses
            north_splitter_direction => top_splitter_direction,
            south_splitter_direction => (OTHERS => '0'),
            east_splitter_direction  => bus_direction,
            west_splitter_direction  => right_splitter_direction,
            -- direction to neibouring busses
            top_instruction_out  => top_instruction_out,
            left_instruction_out => PARTITION_INST_left,
            --			bottom_instruction_out   => bottom_instruction_out, --: out
            right_instruction_out => right_instruction_out, --: out
            --------------------------------------------------------------------------------------------
            -- segmented bus
            --------------------------------------------------------------------------------------------
            HOR_BUS_LEFT_IN   => SRAM_INST_hor_right_out,
            HOR_BUS_RIGHT_IN  => HOR_BUS_RIGHT_IN,
            HOR_BUS_LEFT_OUT  => SRAM_INST_hor_right_in,
            HOR_BUS_RIGHT_OUT => HOR_BUS_RIGHT_OUT,
            VER_BUS_BOTTOM_IN => noc_bus_out_reg,
            VER_BUS_TOP_OUT   => VER_BUS_TOP_OUT,
            --			VER_BUS_BOTTOM_OUT       => VER_BUS_BOTTOM_OUT,
            VER_BUS_TOP_IN => VER_BUS_TOP_IN,
            tb_en          => tb_en,    -- Write Enable from test bench
            tb_addrs       => tb_addrs, -- Write Address from test bench
            tb_inp         => tb_inp,
            tb_ROW         => tb_ROW,
            tb_COL         => tb_COL
        );
END ARCHITECTURE;