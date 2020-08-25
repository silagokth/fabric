-------------------------------------------------------
--! @file STile.vhd
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
-- File       : STile.vhd
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

---------------------------------------------------------------------------------
-- This is a STile which is the second layer of dimarch  
-- it does not have its own sequencer but it recieves instruction for contile sequencer
-- Revisd  segmented bus is replaced with buffers 
---------------------------------------------------------------------------------

LIBRARY IEEE;
LIBRARY work;
USE IEEE.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.top_consts_types_package.ALL;
USE work.noc_types_n_constants.ALL;
USE work.crossbar_types_n_constants.CORSSBAR_INSTRUCTION_RECORD_TYPE;
USE work.crossbar_types_n_constants.nr_of_crossbar_ports;
USE work.misc.ALL;

ENTITY STile IS
    GENERIC
    (
        ID : NATURAL := 0
        --		ROW_WIDTH : integer := 0;         --! Addressing bits for row
        --		COL_WIDTH : integer := 0          --! Addressing bits for col
    );
    PORT
    (
        rst_n : IN STD_LOGIC;
        clk   : IN STD_LOGIC;
        -------------------------
        -- Address signals
        -------------------------
        This_ROW : IN UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! The row address assigned to the cell
        This_COL : IN UNSIGNED(COL_WIDTH - 1 DOWNTO 0); --! The column address assigned to the cell
        -------------------------
        -- crossbar data signals
        -------------------------
        north_in  : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        south_in  : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        east_in   : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        west_in   : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        north_out : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        south_out : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        east_out  : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        west_out  : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        --------------------------------------------------
        -- DIRECTION OF Neibouring buses
        --------------------------------------------------
        north_splitter_direction : IN std_logic_vector(1 DOWNTO 0);
        south_splitter_direction : IN std_logic_vector(1 DOWNTO 0);
        east_splitter_direction  : IN std_logic_vector(1 DOWNTO 0);
        west_splitter_direction  : IN std_logic_vector(1 DOWNTO 0);
        --------------------------------------------------
        -- partitioning instruction to neibouring buses
        --------------------------------------------------
        top_instruction_out    : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        left_instruction_out   : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        bottom_instruction_out : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        right_instruction_out  : OUT PARTITION_INSTRUCTION_RECORD_TYPE;
        ----------------------------
        --	SEMENTED BUS I/0  
        ----------------------------
        HOR_BUS_LEFT_IN    : IN NOC_BUS_TYPE;
        HOR_BUS_RIGHT_IN   : IN NOC_BUS_TYPE;
        HOR_BUS_LEFT_OUT   : OUT NOC_BUS_TYPE;
        HOR_BUS_RIGHT_OUT  : OUT NOC_BUS_TYPE;
        VER_BUS_TOP_IN     : IN NOC_BUS_TYPE;
        VER_BUS_BOTTOM_IN  : IN NOC_BUS_TYPE;
        VER_BUS_TOP_OUT    : OUT NOC_BUS_TYPE;
        VER_BUS_BOTTOM_OUT : OUT NOC_BUS_TYPE;
        --inputs
        --outputs
        --------------------------------------------------------
        --SRAM initialization from testbench
        --------------------------------------------------------
        tb_en    : IN STD_LOGIC;                                         -- Write Enable from test bench
        tb_addrs : IN STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); -- Write Address from test bench
        tb_inp   : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        tb_ROW   : IN UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
        tb_COL   : IN UNSIGNED(COL_WIDTH - 1 DOWNTO 0)
    ); -- Make a new package file and define a constant for this signal
END STile;

ARCHITECTURE behv_rtl OF STile IS
    COMPONENT SRAM IS
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
    END COMPONENT;
    -----------------------------------
    -- SRAM SIGNALS 
    -----------------------------------
    SIGNAL memory_in, memory_out : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);

    -----------------------------------
    -- SRAM AGU SIGNALs 
    -----------------------------------

    SIGNAL SRAM_rw_r, SRAM_rw_w                     : STD_LOGIC;
    SIGNAL SRAM_rw_addrs_out_r, SRAM_rw_addrs_out_w : UNSIGNED(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); --log2_ceil(SRAM_DEPTH)

    SIGNAL DIRECTION_VER : CORSSBAR_INSTRUCTION_RECORD_TYPE;

    SIGNAL AGU_en_r  : std_logic;
    SIGNAL AGU_en_w  : std_logic;
    SIGNAL reorder_r : std_logic;
    SIGNAL reorder_w : std_logic;
    SIGNAL instr_w   : sram_agu_instruction_type;
    SIGNAL instr_r   : sram_agu_instruction_type;
    SIGNAL inp_tmp   : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);         --! Input
    SIGNAL addrs_tmp : STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); -- Write Address from test bench
    SIGNAL en_w_tmp  : std_logic;
BEGIN
    --------------------------
    -- instruction network 
    -------------------------

    U_ISWITCH : ENTITY work.iSwitch(RTL)
        --		generic map(
        --			ROW_WIDTH => ROW_WIDTH,
        --			COL_WIDTH => COL_WIDTH
        --		)
        PORT MAP
        (
            rst_n => rst_n,
            clk   => clk,
            -- address 
            This_ROW => This_ROW,
            This_COL => This_COL,
            ----------
            east_splitter_direction  => east_splitter_direction,
            west_splitter_direction  => west_splitter_direction,
            north_splitter_direction => north_splitter_direction,
            south_splitter_direction => south_splitter_direction,
            HOR_BUS_LEFT_IN          => HOR_BUS_LEFT_IN,
            HOR_BUS_RIGHT_IN         => HOR_BUS_RIGHT_IN,
            VER_BUS_TOP_IN           => VER_BUS_TOP_IN,
            VER_BUS_BOTTOM_IN        => VER_BUS_BOTTOM_IN,
            NORTH_BUS_OUT            => VER_BUS_TOP_OUT,
            SOUTH_BUS_OUT            => VER_BUS_BOTTOM_OUT,
            EAST_BUS_OUT             => HOR_BUS_RIGHT_OUT,
            WEST_BUS_OUT             => HOR_BUS_LEFT_OUT,
            DIRECTION                => DIRECTION_VER,
            top_instruction          => top_instruction_out,
            bottom_instruction       => bottom_instruction_out,
            left_instruction         => left_instruction_out,
            right_instruction        => right_instruction_out,
            SRAM_AGU_instruction_r   => instr_r,
            SRAM_AGU_instruction_w   => instr_w,
            agu_en_r                 => AGU_en_r,
            agu_en_w                 => AGU_en_w
        );

    -- Reg input to the SRAM 
    -- @TODO we need to fix that, the SRAM should be async or in the case of sync SRAM we need the lib file.
    SRAM_input : PROCESS (tb_COL, tb_ROW, This_COL, This_ROW, tb_en, tb_inp, memory_out, tb_addrs, SRAM_rw_addrs_out_w, SRAM_rw_w)
    BEGIN
        IF tb_en = '1' AND tb_COL = This_COL AND tb_ROW = This_ROW THEN
            inp_tmp   <= tb_inp;
            addrs_tmp <= tb_addrs;
            en_w_tmp  <= tb_en;
        ELSE
            inp_tmp   <= memory_out;
            addrs_tmp <= std_logic_vector(SRAM_rw_addrs_out_w);
            en_w_tmp  <= SRAM_rw_w;
        END IF;
    END PROCESS SRAM_input;

    u_sram : ENTITY work.SRAM
        PORT
        MAP(
        CLK      => clk,
        ADDR_W   => addrs_tmp,
        WEB      => en_w_tmp,
        DATA_IN  => inp_tmp,
        ADDR_R   => std_logic_vector(SRAM_rw_addrs_out_r),
        REB      => SRAM_rw_r,
        DATA_OUT => memory_in
        );

    u_crossbar : ENTITY work.data_crossbar(RTL)
        GENERIC
        MAP(
        NumberofPorts => nr_of_crossbar_ports
        )
        PORT
        MAP(
        rst_n     => rst_n,
        clk       => clk,
        DIRECTION => DIRECTION_VER,
        --		VER_SELECT        => DIRECTION_VER,		
        DATA_MEM_IN    => memory_in,
        DATA_NORTH_IN  => north_in,
        DATA_SOUTH_IN  => south_in,
        DATA_EAST_IN   => east_in,
        DATA_WEST_IN   => west_in,
        DATA_MEM_OUT   => memory_out,
        DATA_NORTH_OUT => north_out,
        DATA_SOUTH_OUT => south_out,
        DATA_EAST_OUT  => east_out,
        DATA_WEST_OUT  => west_out
        );

    U_SRAM_AGU_R : ENTITY work.sram_agu
        PORT
        MAP(
        rst_n        => rst_n,
        clk          => clk,
        instr        => instr_r,
        rw           => SRAM_rw_r,
        reorder      => reorder_r,
        rw_addrs_out => SRAM_rw_addrs_out_r
        );

    U_SRAM_AGU_w : ENTITY work.sram_agu
        PORT
        MAP(
        rst_n        => rst_n,
        clk          => clk,
        instr        => instr_w,
        rw           => SRAM_rw_w,
        reorder      => reorder_w,
        rw_addrs_out => SRAM_rw_addrs_out_w
        );

END ARCHITECTURE;