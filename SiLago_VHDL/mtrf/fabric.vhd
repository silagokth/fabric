-------------------------------------------------------
--! @file fabric.vhd
--! @brief 
--! @details 
--! @author Sadiq Hemani
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
-- File       : fabric.vhd
-- Author     : Sadiq Hemani <sadiq@kth.se>
-- Company    : KTH
-- Created    : 2013-09-05
-- Last update: 2013-11-21
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2013
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author                  Description
-- 2013-09-05  1.0      Sadiq Hemani <sadiq@kth.se>
-- 2013-09-30  1.0      Nasim Farahini <farahini@kth.se>
-- 2013-11-21 1.0       Sadiq Hemani <sadiq@kth.se>
-- 2014-02-10  1.0      Nasim Farahini <farahini@kth.se>
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
USE work.seq_functions_package.ALL;
USE work.util_package.ALL;
--USE ieee.std_logic_unsigned.ALL;
USE work.top_consts_types_package.ALL;
USE work.noc_types_n_constants.ALL;
USE work.crossbar_types_n_constants.ALL;

ENTITY fabric IS
    PORT (
        clk       : IN std_logic;
        rst_n     : IN std_logic;
        instr_ld  : IN std_logic;                                  --std_logic_vector(COLUMNS - 1 downto 0);
        instr_inp : IN std_logic_vector(INSTR_WIDTH - 1 DOWNTO 0); --v_bus_signal_ty;--std_logic_vector(OLD_INSTR_WIDTH-1 DOWNTO 0);
        --seq_address : IN std_logic_vector(SEQ_ADDRS_WIDTH-1 DOWNTO 0)
        seq_address_rb : IN std_logic_vector(ROWS - 1 DOWNTO 0);    ---in order to generate addresses for sequencer
        seq_address_cb : IN std_logic_vector(COLUMNS - 1 DOWNTO 0); ---in order to generate addresses for sequencer

        dir_north_out_array : OUT DATA_IO_SIGNAL_TYPE(0 TO COLUMNS - 1);
        dir_south_in_array  : IN DATA_IO_SIGNAL_TYPE(0 TO COLUMNS - 1);

        top_instruction_out_array : OUT PARTITION_INSTRUCTION_TYPE(0 TO COLUMNS - 1, 0 TO 0);
        top_splitter_dir_in_array : IN DIRECTION_TYPE(0 TO COLUMNS - 1, 0 TO 0);

        SRAM_INST_ver_top_out : OUT INST_SIGNAL_TYPE(0 TO COLUMNS - 1, 0 TO 0);
        SRAM_INST_ver_top_in  : IN INST_SIGNAL_TYPE(0 TO COLUMNS - 1, 0 TO 0);
        --------------------------------------------------------
        --SRAM initialization from testbench
        --------------------------------------------------------
        tb_en    : IN STD_LOGIC;                                         -- Write Enable from test bench
        tb_addrs : IN STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); -- Write Address from test bench
        tb_inp   : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);
        tb_ROW   : IN UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
        tb_COL   : IN UNSIGNED(COL_WIDTH - 1 DOWNTO 0)
        -- k: out k_test(0 TO COLUMNS-1, 0 TO ROWS-1)
    );
END ENTITY fabric;

ARCHITECTURE rtl OF fabric IS
    TYPE mem_address_ty IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 DOWNTO 0);
    TYPE mem_output_ty IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF signed(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
    SIGNAL reg_write : SRAM_RW_ENABLE_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);

    SIGNAL reg_in         : SRAM_RW_DATA_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);
    SIGNAL reg_out_signed : SRAM_RW_SIGNED_DATA_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);

    --SIGNAL hc_out_left_w  : hc_out_w_ty(0 TO COLUMNS-1, 0 TO ROWS-1);
    --SIGNAL hc_out_right_w : hc_out_w_ty(0 TO COLUMNS-1, 0 TO ROWS-1);
    SIGNAL s_bus_out_w      : s_bus_out_ty(0 TO COLUMNS - 1, 0 TO ROWS - 1);
    SIGNAL sel_config_w_reg : s_bus_switchbox_2d_ty(0 TO COLUMNS - 1, 0 TO ROWS - 1);
    SIGNAL sel_config_w_dpu : s_bus_switchbox_2d_ty(0 TO COLUMNS - 1, 0 TO ROWS - 1);
    SIGNAL v_lanes          : v_lane_ty(0 TO COLUMNS - 1);

    ---------------------------------------------
    -- NOC SIGNALS
    ---------------------------------------------

    SIGNAL DATA_DIR_SOUTH : DATA_SIGNAL_TYPE(0 TO COLUMNS - 1, 0 TO MAX_ROW);
    SIGNAL DATA_DIR_NORTH : DATA_SIGNAL_TYPE(0 TO COLUMNS - 1, 0 TO MAX_ROW);
    SIGNAL DATA_DIR_WEST  : DATA_SIGNAL_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL DATA_DIR_EAST  : DATA_SIGNAL_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);

    --  signal SRAM_INST_hor : INST_SIGNAL_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    --  signal SRAM_INST_ver : INST_SIGNAL_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    --signal SRAM_INST_hor_left_in    : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_hor_right_in   : INST_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW);
    --signal SRAM_INST_hor_top_in     : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_hor_bottom_in  : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_hor_left_out   : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_hor_right_out  : INST_SIGNAL_TYPE(0 to COLUMNS+1, 0 to MAX_ROW);
    --signal SRAM_INST_hor_top_out    : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_hor_bottom_out : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);

    --signal SRAM_INST_ver_left_in   : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_ver_right_in  : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --  signal SRAM_INST_ver_top_in : INST_SIGNAL_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    --  signal SRAM_INST_ver_bottom_in : INST_SIGNAL_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    --signal SRAM_INST_ver_left_out  : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_ver_right_out : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --  signal SRAM_INST_ver_top_out : INST_SIGNAL_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    --  signal SRAM_INST_ver_bottom_out : INST_SIGNAL_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);

    --	signal SRAM_INST_ver_in                    : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --signal SRAM_INST_hor_in, SRAM_INST_hor_out : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);
    --	signal SRAM_INST_ver_out                   : INST_SIGNAL_TYPE(0 to COLUMNS, 0 to MAX_ROW);

    SIGNAL SRAM_INST_hor_left_dir_array  : INST_SIGNAL_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL SRAM_INST_hor_right_dir_array : INST_SIGNAL_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);

    -- signal splitter_priority_ver  : PRIORITY_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    SIGNAL splitter_direction_ver : DIRECTION_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    SIGNAL splitter_direction_hor : DIRECTION_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    --  
    --  signal splitter_priority_hor  : PRIORITY_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);

    SIGNAL PARTITION_INST_top    : PARTITION_INSTRUCTION_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    SIGNAL PARTITION_INST_bottom : PARTITION_INSTRUCTION_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    SIGNAL PARTITION_INST_left   : PARTITION_INSTRUCTION_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);
    SIGNAL PARTITION_INST_right  : PARTITION_INSTRUCTION_TYPE(0 TO COLUMNS, 0 TO MAX_ROW);

    SIGNAL DATA_SEQ_IN  : CROSSBAR_DATA_TYPE(0 TO COLUMNS - 1);
    SIGNAL DATA_SEQ_OUT : CROSSBAR_DATA_TYPE(0 TO COLUMNS - 1);

    -- lcc signals 

    SIGNAL MLFC_CONFIG : LOADER_ARRAY_TYPE(COLUMNS - 1 DOWNTO 0);
    SIGNAL row_sel     : row_sel_ty;
    --    signal instr_output   : v_bus_signal_ty;
    --   signal row_sel_replace: std_logic;
    --signal	fr_lcc_w, fr_lcc_arbiter_w, to_lcc_arbiter_w, to_lcc_w, to_memseq_w, fr_memseq_w :lcc_elements_ty;	
    --signal seq_address_rb_mlfc_out : std_logic_vector(ROWS downto 0); ---in order to generate addresses for sequencer
    --signal seq_address_cb_mlfc_out : std_logic_vector(COLUMNS - 1 downto 0); ---in order to generate addresses for sequencer

    SIGNAL h_bus_reg_seg_0 : h_bus_seg_ty(0 TO 2 * COLUMNS + 1, 0 TO 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus register file output 0
    SIGNAL h_bus_reg_seg_1 : h_bus_seg_ty(0 TO 2 * COLUMNS + 1, 0 TO 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus register file output 1
    SIGNAL h_bus_dpu_seg_0 : h_bus_seg_ty(0 TO 2 * COLUMNS + 1, 0 TO 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus dpu output 0
    SIGNAL h_bus_dpu_seg_1 : h_bus_seg_ty(0 TO 2 * COLUMNS + 1, 0 TO 2 * MAX_NR_OF_OUTP_N_HOPS); --horizontal bus dpu output 1
    SIGNAL sel_r_seg       : s_bus_switchbox_2d_ty(0 TO COLUMNS - 1, 0 TO ROWS - 1);
    SIGNAL v_bus           : v_bus_ty_2d(0 TO COLUMNS - 1, 0 TO ROWS - 1);
    --signal bus_direction_hor : PARTITION_INSTRUCTION_STATUS_TYPE(0 to COLUMNS+1, 0 to MAX_ROW+1);
    SIGNAL bus_direction_ver       : PARTITION_INSTRUCTION_STATUS_TYPE(0 TO COLUMNS + 1, 0 TO MAX_ROW + 1);
    SIGNAL noc_bus_out             : INST_SIGNAL_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1); -- previous was only 0 to COLUMNS
    SIGNAL dimarch_silego_data_in  : DATA_SIGNAL_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1);
    SIGNAL dimarch_silego_data_out : DATA_SIGNAL_TYPE(0 TO COLUMNS - 1, 0 TO ROWS - 1); -- previous was 0 to ROWS-1
    SIGNAL dimarch_silego_rd_out   : DATA_RD_TYPE(0 TO COLUMNS - 1, 0 TO 0);            -- previous second argument was 0 to ROWS-1

    -------------------------
    -- Address signals
    -------------------------	
    -- DiMArch 
    SIGNAL row_dimarch_right        : ROW_ADDR_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL col_dimarch_right        : COL_ADDR_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL addr_valid_dimarch_right : ADDR_VALID_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);

    -- DRRA
    SIGNAL addr_valid_bot   : ADDR_VALID_TYPE(0 TO 0, 0 TO ROWS - 2);
    SIGNAL addr_valid_right : ADDR_VALID_TYPE(0 TO COLUMNS - 2, 0 TO ROWS - 1);
    SIGNAL addr_valid_top   : ADDR_VALID_TYPE(0 TO 0, 0 TO MAX_ROW - 1);
    SIGNAL row_bot          : ROW_ADDR_TYPE(0 TO 0, 0 TO ROWS - 2);
    SIGNAL row_right        : ROW_ADDR_TYPE(0 TO COLUMNS - 2, 0 TO ROWS - 1);
    SIGNAL row_top          : ROW_ADDR_TYPE(0 TO 0, 0 TO MAX_ROW - 1);
    SIGNAL col_bot          : COL_ADDR_TYPE(0 TO 0, 0 TO ROWS - 2);
    SIGNAL col_right        : COL_ADDR_TYPE(0 TO COLUMNS - 2, 0 TO ROWS - 1);
    SIGNAL col_top          : COL_ADDR_TYPE(0 TO 0, 0 TO MAX_ROW - 1);

    -------------------------
    -- Instruction signals
    -------------------------
    SIGNAL instr_ld_right       : DATA_RD_TYPE(0 TO COLUMNS - 2, 0 TO ROWS - 1);
    SIGNAL instr_ld_bot         : DATA_RD_TYPE(0 TO 0, 0 TO ROWS - 2);
    SIGNAL instr_inp_right      : INSTR_ARRAY_TYPE(0 TO COLUMNS - 2, 0 TO ROWS - 1);
    SIGNAL instr_inp_bot        : INSTR_ARRAY_TYPE(0 TO 0, 0 TO ROWS - 2);
    SIGNAL seq_address_rb_right : SEQ_ADDR_RB_ARRAY_TYPE(0 TO COLUMNS - 2, 0 TO ROWS - 1);
    SIGNAL seq_address_rb_bot   : SEQ_ADDR_RB_ARRAY_TYPE(0 TO 0, 0 TO ROWS - 2);
    SIGNAL seq_address_cb_right : SEQ_ADDR_CB_ARRAY_TYPE(0 TO COLUMNS - 2, 0 TO ROWS - 1);
    SIGNAL seq_address_cb_bot   : SEQ_ADDR_CB_ARRAY_TYPE(0 TO 0, 0 TO ROWS - 2);

    -------------------------
    -- DiMArch signals
    -------------------------
    --signal dir_north_out_array      STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); -- OUT
    --signal north_in_array       STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); -- IN
    --signal top_splitter_direction : std_logic_vector(1 DOWNTO 0); --!  IN , comes from the top (south splitter)
    --signal top_instruction_out       : PARTITION_INSTRUCTION_RECORD_TYPE; -- OUT
    --signal VER_BUS_TOP_IN            : NOC_BUS_TYPE; -- IN 
    --signal VER_BUS_TOP_OUT           : NOC_BUS_TYPE; -- OUT 

    SIGNAL left_splitter_dir_array : DIRECTION_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL instruction_dir_right   : PARTITION_INSTRUCTION_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL instruction_dir_top     : PARTITION_INSTRUCTION_TYPE(0 TO COLUMNS - 1, 0 TO MAX_ROW - 1);
    -------------------------
    -- SRAM testbench signals
    -------------------------
    SIGNAL tb_en_out_array    : DATA_RD_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL tb_addrs_out_array : SRAM_ADDR_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL tb_inp_out_array   : DATA_SIGNAL_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL tb_ROW_out_array   : SRAM_ROW_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);
    SIGNAL tb_COL_out_array   : SRAM_COL_TYPE(0 TO COLUMNS - 2, 0 TO MAX_ROW - 1);

BEGIN -- ARCHITECTURE rtl

    --------------------------------------------------------------------------------------------------
    -- DiMArch
    --------------------------------------------------------------------------------------------------	
    --DATA_DIR_WEST(COLUMNS, 0)<=(others =>'0'); -- data west input tied to zero (REMOVE)
    --DATA_DIR_EAST(0, 0) <= (others=>'0'); -- data east input tied to zero (REMOVE)
    --SRAM_INST_hor_right_in (1,0) <= IDLE_BUS;	
    --SRAM_INST_hor_right_out(0,0) <= IDLE_BUS;
    --bus_direction_hor(COLUMNS,0) <= (others => '0');
    --DiMArch_COLS
    DiMArch_COLS : FOR i IN 0 TO COLUMNS - 1 GENERATE
    BEGIN

        --SRAM_INST_ver_right_out(i, MAX_ROW-1) <= noc_bus_out(i,0);

        --DATA_DIR_SOUTH(i,  MAX_ROW) <= (others=>'0'); -- data north input tied to zero (REMOVE)

        --SRAM_INST_ver_left_in  (i, MAX_ROW-1) <=IDLE_BUS;
        --SRAM_INST_ver_right_in (i,MAX_ROW-1) <=IDLE_BUS;
        --bus_direction_ver(i,MAX_ROW) <= (others =>'0');
        --bus_direction_ver(i, 0) <=(others => '0');
        --SRAM_INST_ver_left_out(i,0)<= IDLE_BUS;

        top_instruction_out_array(i, 0) <= instruction_dir_top(i, 0);
        dir_north_out_array(i)          <= DATA_DIR_NORTH(i, MAX_ROW);
        DATA_DIR_SOUTH(i, 0)            <= dir_south_in_array(i);

        --DiMArch_ROWS
        DiMArch_ROWS : FOR j IN 0 TO MAX_ROW - 1 GENERATE
        BEGIN
            if_dimarch_bot_l_corner : IF j = (MAX_ROW - 1) AND i = 0 GENERATE
                DiMArchTile_bot_l_inst : ENTITY work.DiMArchTile_Bot_left_corner
                    PORT MAP
                    (

                        rst_n => rst_n,
                        clk   => clk,

                        -------------------------
                        -- Address signals
                        -------------------------
                        --		start_row                 : in  std_logic; --! Start signal (connected to the valid signal of the previous block in the same row)
                        start_col => addr_valid_top(i, 0),           --! Start signal (connected to the valid signal of the previous block in the same col)
                        prevRow   => row_top(i, 0),                  --! Row address assigned to the previous cell
                        prevCol   => col_top(i, 0),                  --! Col address assigned to the previous cell
                        valid     => addr_valid_dimarch_right(i, j), --! Valid signals, used to signal that the assignment of the address is complete
                        thisRow   => row_dimarch_right(i, j),        --! The row address assigned to the cell
                        thisCol   => col_dimarch_right(i, j),        --! The column address assigned to the cell
                        -------------------------
                        -- crossbar data signals
                        -------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        north_out => DATA_DIR_NORTH(i, j + 1),
                        north_in  => DATA_DIR_SOUTH(i, j),
                        ----------------------------
                        south_in => DATA_DIR_NORTH(i, j),
                        east_in  => DATA_DIR_WEST(i, j),
                        --west_in         => DATA_DIR_EAST(i-1,j),
                        south_out => DATA_DIR_SOUTH(i, j + 1),
                        east_out  => DATA_DIR_EAST(i, j),
                        --west_out        => DATA_DIR_WEST(i-1,j),
                        --------------------------------------------------
                        -- DIRECTION OF Neibouring buses
                        --------------------------------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        top_splitter_direction => top_splitter_dir_in_array(i, 0), --! comes from the top (south splitter)
                        ------------------------------		south_splitter_direction  : in  std_logic_vector(1 DOWNTO 0);
                        right_splitter_direction => left_splitter_dir_array(i, j), --! comes from the right(west splitter)
                        --left_splitter_direction   => left_splitter_dir_array(i-1,j), --! goes from the seg_bus to the left (west splitter output)
                        --------------------------------------------------
                        -- partitioning instruction to neibouring buses
                        --------------------------------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        top_instruction_out => instruction_dir_top(i, j),
                        ----------------------------		
                        --left_instruction_in       => instruction_dir_right(i-1,j),
                        --		bottom_instruction_out    : out PARTITION_INSTRUCTION_RECORD_TYPE;
                        right_instruction_out => instruction_dir_right(i, j),
                        ----------------------------
                        --	SEMENTED BUS I/0  
                        ----------------------------
                        --HOR_BUS_LEFT_IN         => SRAM_INST_hor_right_dir_array(i-1, j),
                        HOR_BUS_RIGHT_IN => SRAM_INST_hor_left_dir_array(i, j),
                        --HOR_BUS_LEFT_OUT        => SRAM_INST_hor_left_dir_array(i-1,j),
                        HOR_BUS_RIGHT_OUT => SRAM_INST_hor_right_dir_array(i, j),
                        VER_BUS_BOTTOM_IN => noc_bus_out(i, 0),
                        --VER_BUS_BOTTOM_IN_0       : in  NOC_BUS_TYPE;
                        --VER_BUS_BOTTOM_IN_1       : in  NOC_BUS_TYPE;
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        VER_BUS_TOP_IN  => SRAM_INST_ver_top_in(i, 0),
                        VER_BUS_TOP_OUT => SRAM_INST_ver_top_out(i, 0),
                        ----------------------------
                        --	VER_BUS_BOTTOM_OUT        : out NOC_BUS_TYPE;
                        --inputs
                        --outputs
                        --------------------------------------------------------
                        --SRAM initialization from testbench -- input signals from the left hand side
                        --------------------------------------------------------
                        tb_en    => tb_en,    -- Write Enable from test bench
                        tb_addrs => tb_addrs, -- Write Address from test bench
                        tb_inp   => tb_inp,
                        tb_ROW   => tb_ROW,
                        tb_COL   => tb_COL,
                        --------------------------------------------------------
                        --SRAM initialization from testbench -- output signals from the right hand side
                        --------------------------------------------------------
                        tb_en_out    => tb_en_out_array(i, j),
                        tb_addrs_out => tb_addrs_out_array(i, j), -- Write Address from test bench
                        tb_inp_out   => tb_inp_out_array(i, j),
                        tb_ROW_out   => tb_ROW_out_array(i, j),
                        tb_COL_out   => tb_COL_out_array(i, j)
                    ); -- Make a new package file and define a constant for this signal
            END GENERATE;

            if_dimarch_bot : IF j = (MAX_ROW - 1) AND i > 0 AND i < (COLUMNS - 1) GENERATE
                DiMArchTile_bot_inst : ENTITY work.DiMArchTile_Bot
                    PORT MAP(

                        rst_n => rst_n,
                        clk   => clk,

                        -------------------------
                        -- Address signals
                        -------------------------
                        start_row => addr_valid_dimarch_right(i - 1, j), --! Start signal (connected to the valid signal of the previous block in the same row)
                        --start_col                 : in  std_logic; --! Start signal (connected to the valid signal of the previous block in the same col)
                        prevRow => row_dimarch_right(i - 1, j),    --! Row address assigned to the previous cell
                        prevCol => col_dimarch_right(i - 1, j),    --! Col address assigned to the previous cell
                        valid   => addr_valid_dimarch_right(i, j), --! Valid signals, used to signal that the assignment of the address is complete
                        thisRow => row_dimarch_right(i, j),        --! The row address assigned to the cell
                        thisCol => col_dimarch_right(i, j),        --! The column address assigned to the cell
                        -------------------------
                        -- crossbar data signals
                        -------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        north_out => DATA_DIR_NORTH(i, j + 1),
                        north_in  => DATA_DIR_SOUTH(i, j),
                        ----------------------------
                        south_in  => DATA_DIR_NORTH(i, j),
                        east_in   => DATA_DIR_WEST(i, j),
                        west_in   => DATA_DIR_EAST(i - 1, j),
                        south_out => DATA_DIR_SOUTH(i, j + 1),
                        east_out  => DATA_DIR_EAST(i, j),
                        west_out  => DATA_DIR_WEST(i - 1, j),
                        --------------------------------------------------
                        -- DIRECTION OF Neibouring buses
                        --------------------------------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        top_splitter_direction => top_splitter_dir_in_array(i, 0), --! comes from the top (south splitter)
                        ------------------------------		south_splitter_direction  : in  std_logic_vector(1 DOWNTO 0);
                        right_splitter_direction => left_splitter_dir_array(i, j),     --! comes from the right(west splitter)
                        left_splitter_direction  => left_splitter_dir_array(i - 1, j), --! goes from the seg_bus to the left (west splitter output)
                        --------------------------------------------------
                        -- partitioning instruction to neibouring buses
                        --------------------------------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        top_instruction_out => instruction_dir_top(i, j),
                        ----------------------------
                        left_instruction_in => instruction_dir_right(i - 1, j),
                        --		bottom_instruction_out    : out PARTITION_INSTRUCTION_RECORD_TYPE;
                        right_instruction_out => instruction_dir_right(i, j),
                        ----------------------------
                        --	SEMENTED BUS I/0  
                        ----------------------------
                        HOR_BUS_LEFT_IN   => SRAM_INST_hor_right_dir_array(i - 1, j),
                        HOR_BUS_RIGHT_IN  => SRAM_INST_hor_left_dir_array(i, j),
                        HOR_BUS_LEFT_OUT  => SRAM_INST_hor_left_dir_array(i - 1, j),
                        HOR_BUS_RIGHT_OUT => SRAM_INST_hor_right_dir_array(i, j),
                        VER_BUS_BOTTOM_IN => noc_bus_out(i, 0),
                        --VER_BUS_BOTTOM_IN_0       : in  NOC_BUS_TYPE;
                        --VER_BUS_BOTTOM_IN_1       : in  NOC_BUS_TYPE;
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        VER_BUS_TOP_IN  => SRAM_INST_ver_top_in(i, 0),
                        VER_BUS_TOP_OUT => SRAM_INST_ver_top_out(i, 0),
                        ----------------------------
                        --	VER_BUS_BOTTOM_OUT        : out NOC_BUS_TYPE;
                        --inputs
                        --outputs
                        --------------------------------------------------------
                        --SRAM initialization from testbench -- input signals from the left hand side
                        --------------------------------------------------------
                        tb_en    => tb_en_out_array(i - 1, j),    -- Write Enable from test bench
                        tb_addrs => tb_addrs_out_array(i - 1, j), -- Write Address from test bench
                        tb_inp   => tb_inp_out_array(i - 1, j),
                        tb_ROW   => tb_ROW_out_array(i - 1, j),
                        tb_COL   => tb_COL_out_array(i - 1, j),
                        --------------------------------------------------------
                        --SRAM initialization from testbench -- output signals from the right hand side
                        --------------------------------------------------------
                        tb_en_out    => tb_en_out_array(i, j),
                        tb_addrs_out => tb_addrs_out_array(i, j), -- Write Address from test bench
                        tb_inp_out   => tb_inp_out_array(i, j),
                        tb_ROW_out   => tb_ROW_out_array(i, j),
                        tb_COL_out   => tb_COL_out_array(i, j)
                    ); -- Make a new package file and define a constant for this signal
            END GENERATE;

            if_dimarch_bot_r_corner : IF j = (MAX_ROW - 1) AND (i = COLUMNS - 1) GENERATE
                DiMArchTile_bot_r_inst : ENTITY work.DiMArchTile_Bot_right_corner
                    PORT MAP(
                        rst_n => rst_n,
                        clk   => clk,
                        -------------------------
                        -- Address signals
                        -------------------------
                        start_row => addr_valid_dimarch_right(i - 1, j), --! Start signal (connected to the valid signal of the previous block in the same row)
                        --start_col                 : in  std_logic; --! Start signal (connected to the valid signal of the previous block in the same col)
                        prevRow => row_dimarch_right(i - 1, j), --! Row address assigned to the previous cell
                        prevCol => col_dimarch_right(i - 1, j), --! Col address assigned to the previous cell
                        -------------------------
                        -- crossbar data signals
                        -------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        north_out => DATA_DIR_NORTH(i, j + 1),
                        north_in  => DATA_DIR_SOUTH(i, j),
                        ----------------------------
                        south_in => DATA_DIR_NORTH(i, j),
                        --east_in         => DATA_DIR_WEST(i,j),
                        west_in   => DATA_DIR_EAST(i - 1, j),
                        south_out => DATA_DIR_SOUTH(i, j + 1),
                        --east_out        => DATA_DIR_EAST(i,j),
                        west_out => DATA_DIR_WEST(i - 1, j),
                        --------------------------------------------------
                        -- DIRECTION OF Neibouring buses
                        --------------------------------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        top_splitter_direction => top_splitter_dir_in_array(i, 0), --! comes from the top (south splitter)
                        ------------------------------		south_splitter_direction  : in  std_logic_vector(1 DOWNTO 0);
                        --right_splitter_direction  	=> left_splitter_dir_array(i,j), --! comes from the right(west splitter)
                        left_splitter_direction => left_splitter_dir_array(i - 1, j), --! goes from the seg_bus to the left (west splitter output)
                        --------------------------------------------------
                        -- partitioning instruction to neibouring buses
                        --------------------------------------------------
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        top_instruction_out => instruction_dir_top(i, j),
                        ----------------------------		
                        left_instruction_in => instruction_dir_right(i - 1, j),
                        --		bottom_instruction_out    : out PARTITION_INSTRUCTION_RECORD_TYPE;
                        --right_instruction_out   : out PARTITION_INSTRUCTION_RECORD_TYPE;
                        ----------------------------
                        --	SEMENTED BUS I/0  
                        ----------------------------
                        HOR_BUS_LEFT_IN => SRAM_INST_hor_right_dir_array(i - 1, j),
                        --HOR_BUS_RIGHT_IN        => SRAM_INST_hor_left_dir_array(i,j),
                        HOR_BUS_LEFT_OUT => SRAM_INST_hor_left_dir_array(i - 1, j),
                        --HOR_BUS_RIGHT_OUT       => SRAM_INST_hor_right_dir_array(i,j),
                        VER_BUS_BOTTOM_IN => noc_bus_out(i, 0),
                        --VER_BUS_BOTTOM_IN_0       : in  NOC_BUS_TYPE;
                        --VER_BUS_BOTTOM_IN_1       : in  NOC_BUS_TYPE;
                        ----------------------------
                        -- Top Signals not in used
                        ----------------------------
                        VER_BUS_TOP_IN  => SRAM_INST_ver_top_in(i, 0),
                        VER_BUS_TOP_OUT => SRAM_INST_ver_top_out(i, 0),
                        ----------------------------
                        --	VER_BUS_BOTTOM_OUT        : out NOC_BUS_TYPE;
                        --inputs
                        --outputs
                        --------------------------------------------------------
                        --SRAM initialization from testbench -- input signals from the left hand side
                        --------------------------------------------------------
                        tb_en    => tb_en_out_array(i - 1, j),    -- Write Enable from test bench
                        tb_addrs => tb_addrs_out_array(i - 1, j), -- Write Address from test bench
                        tb_inp   => tb_inp_out_array(i - 1, j),
                        tb_ROW   => tb_ROW_out_array(i - 1, j),
                        tb_COL   => tb_COL_out_array(i - 1, j)
                    ); -- Make a new package file and define a constant for this signal
            END GENERATE;

        END GENERATE DiMArch_ROWS;
    END GENERATE DiMArch_COLS;

    --------------------------------------------------------------------------------------------------
    -- DRRA
    --------------------------------------------------------------------------------------------------	

    --! Assigns data from DRRA to bottom rows of DiMArch, and data from DiMArch to top row of DRRA
    inp_assignloop : FOR i IN 0 TO COLUMNS - 1 GENERATE
        DATA_DIR_NORTH(i, 0)         <= dimarch_silego_data_out(i, 0);
        dimarch_silego_data_in(i, 0) <= DATA_DIR_SOUTH(i, MAX_ROW);
    END GENERATE;

    MTRF_COLS : FOR i IN 0 TO COLUMNS - 1 GENERATE

    BEGIN

        MTRF_ROWS : FOR j IN 0 TO ROWS - 1 GENERATE

        BEGIN

            --h_bus_reg_seg_0(0, 0) <= (others => '0');
            --h_bus_reg_seg_0(0, 1) <= (others => '0');
            --h_bus_reg_seg_1(0, 0) <= (others => '0');
            --h_bus_reg_seg_1(0, 1) <= (others => '0');
            --h_bus_dpu_seg_0(0, 0) <= (others => '0');
            --h_bus_dpu_seg_0(0, 1) <= (others => '0');
            --h_bus_dpu_seg_1(0, 0) <= (others => '0');
            --h_bus_dpu_seg_1(0, 1) <= (others => '0');

            --h_bus_reg_seg_0(1, 0) <= (others => '0');
            --h_bus_reg_seg_0(1, 1) <= (others => '0');
            --h_bus_reg_seg_1(1, 0) <= (others => '0');
            --h_bus_reg_seg_1(1, 1) <= (others => '0');
            --h_bus_dpu_seg_0(1, 0) <= (others => '0');
            --h_bus_dpu_seg_0(1, 1) <= (others => '0');
            --h_bus_dpu_seg_1(1, 0) <= (others => '0');
            --h_bus_dpu_seg_1(1, 1) <= (others => '0');

            --h_bus_reg_seg_0(2 * COLUMNS, 3) <= (others => '0');
            --h_bus_reg_seg_0(2 * COLUMNS, 4) <= (others => '0');
            --h_bus_reg_seg_1(2 * COLUMNS, 3) <= (others => '0');
            --h_bus_reg_seg_1(2 * COLUMNS, 4) <= (others => '0');
            --h_bus_dpu_seg_0(2 * COLUMNS, 3) <= (others => '0');
            --h_bus_dpu_seg_0(2 * COLUMNS, 4) <= (others => '0');
            --h_bus_dpu_seg_1(2 * COLUMNS, 3) <= (others => '0');
            --h_bus_dpu_seg_1(2 * COLUMNS, 4) <= (others => '0');

            --h_bus_reg_seg_0(2 * COLUMNS + 1, 3) <= (others => '0');
            --h_bus_reg_seg_0(2 * COLUMNS + 1, 4) <= (others => '0');
            --h_bus_reg_seg_1(2 * COLUMNS + 1, 3) <= (others => '0');
            --h_bus_reg_seg_1(2 * COLUMNS + 1, 4) <= (others => '0');
            --h_bus_dpu_seg_0(2 * COLUMNS + 1, 3) <= (others => '0');
            --h_bus_dpu_seg_0(2 * COLUMNS + 1, 4) <= (others => '0');
            --h_bus_dpu_seg_1(2 * COLUMNS + 1, 3) <= (others => '0');
            --h_bus_dpu_seg_1(2 * COLUMNS + 1, 4) <= (others => '0');

            if_drra_top_l_corner : IF j = 0 AND i = 0 GENERATE -- top row, corners
                Silago_top_l_corner_inst : ENTITY work.Silago_top_left_corner
                    PORT MAP(
                        clk   => clk,
                        rst_n => rst_n,

                        -------------------------
                        -- Address signals
                        -------------------------
                        --start       => '1', --! Start signal (connected to the valid signal of the previous block in the same row)
                        --		start_col                    : in  std_logic; --! Start signal (connected to the valid signal of the previous block in the same col)
                        --		prevRow                      : in  UNSIGNED(ROW_bit - 1 DOWNTO 0); --! Row address assigned to the previous cell
                        --		prevCol                      : in  UNSIGNED(COL_bit - 1 DOWNTO 0); --! Col address assigned to the previous cell

                        valid_top   => addr_valid_top(i, j), --! Valid signals, used to signal that the assignment of the address is complete
                        thisRow_top => row_top(i, j),        --! The row address assigned to the cell
                        thisCol_top => col_top(i, j),        --! The column address assigned to the cell

                        valid_right   => addr_valid_right(i, j), --! Valid signals, used to signal that the assignment of the address is complete
                        thisRow_right => row_right(i, j),        --! The row address assigned to the cell
                        thisCol_right => col_right(i, j),        --! The column address assigned to the cell

                        valid_bot   => addr_valid_bot(i, j), --! Copy of the valid signal, connection to the bottom row
                        thisRow_bot => row_bot(i, j),        --! Copy of the row signal, connection to the bottom row
                        thisCol_bot => col_bot(i, j),        --! Copy of the col signal, connection to the bottom row
                        ------------------------------
                        -- Data in (from next row)
                        ------------------------------
                        data_in_next               => dimarch_silego_data_out(i, j + 1), --! data from other row
                        dimarch_silego_rd_out_next => dimarch_silego_rd_out(i, 0),       --! ready signal from the other row

                        ------------------------------
                        -- Data out to DiMArch
                        ------------------------------
                        dimarch_data_in_out => dimarch_silego_data_in(i, j + 1), --! data from DiMArch to the next row

                        ------------------------------
                        -- Global signals for configuration
                        ------------------------------
                        -- inputs (left hand side)
                        instr_ld       => instr_ld,       --! load instruction signal
                        instr_inp      => instr_inp,      --! Actual instruction to be loaded
                        seq_address_rb => seq_address_rb, --! in order to generate addresses for sequencer rows
                        seq_address_cb => seq_address_cb, --! in order to generate addresses for sequencer cols
                        -- outputs (right hand side)
                        instr_ld_out_right       => instr_ld_right(i, j),       --! Registered instruction load signal, broadcast to the next cell
                        instr_inp_out_right      => instr_inp_right(i, j),      --! Registered instruction signal, bradcast to the next cell
                        seq_address_rb_out_right => seq_address_rb_right(i, j), --! registered signal, broadcast to the next cell, in order to generate addresses for sequencer rows
                        seq_address_cb_out_right => seq_address_cb_right(i, j), --! registed signal, broadcast to the next cell, in order to generate addresses for sequencer cols

                        instr_ld_out_bot       => instr_ld_bot(i, j),       --! Registered instruction load signal, broadcast to the next cell
                        instr_inp_out_bot      => instr_inp_bot(i, j),      --! Registered instruction signal, bradcast to the next cell
                        seq_address_rb_out_bot => seq_address_rb_bot(i, j), --! registered signal, broadcast to the next cell, in order to generate addresses for sequencer rows
                        seq_address_cb_out_bot => seq_address_cb_bot(i, j), --! registed signal, broadcast to the next cell, in order to generate addresses for sequencer cols
                        ------------------------------
                        -- Silego core cell
                        ------------------------------
                        --RegFile
                        -- Data transfer only allowed through the dimarch
                        dimarch_data_in  => dimarch_silego_data_in(i, j),  --! data from dimarch (top)
                        dimarch_data_out => dimarch_silego_data_out(i, j), --! data out to dimarch (top)
                        ------------------------------
                        -- DiMArch bus output
                        ------------------------------
                        noc_bus_out => noc_bus_out(i, j), --! noc bus signal to the dimarch (top)
                        ------------------------------
                        -- NoC bus from the next row to the DiMArch
                        ------------------------------
                        -- TODO we can move the noc bus selector from the DiMArch to the cell in order to save some routing
                        noc_bus_in => noc_bus_out(i, j + 1), --! noc bus signal from the adjacent row (bottom)
                        ------------------------------
                        --Horizontal Busses
                        ------------------------------
                        ---------------------------------------------------------------------------------------
                        -- Modified by Dimitris to remove inputs and outputs that are not connected (left hand side)
                        -- Date 15/03/2018
                        ---------------------------------------------------------------------------------------
                        --Horizontal Busses
                        --h_bus_reg_in_out0_0_left   => h_bus_reg_seg_0(2 * i + j, 0),
                        --h_bus_reg_in_out0_1_left   => h_bus_reg_seg_0(2 * i + j, 1), 
                        h_bus_reg_in_out0_3_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out0_4_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out0_0_right => h_bus_reg_seg_0(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out0_1_right => h_bus_reg_seg_0(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out0_3_left  => h_bus_reg_seg_0(2 * i + j, 3),
                        h_bus_reg_out_out0_4_left  => h_bus_reg_seg_0(2 * i + j, 4),
                        --h_bus_reg_in_out1_0_left   => h_bus_reg_seg_1(2 * i + j, 0),
                        --h_bus_reg_in_out1_1_left   => h_bus_reg_seg_1(2 * i + j, 1),
                        h_bus_reg_in_out1_3_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out1_4_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out1_0_right => h_bus_reg_seg_1(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out1_1_right => h_bus_reg_seg_1(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out1_3_left  => h_bus_reg_seg_1(2 * i + j, 3),
                        h_bus_reg_out_out1_4_left  => h_bus_reg_seg_1(2 * i + j, 4),

                        --h_bus_dpu_in_out0_0_left   => h_bus_dpu_seg_0(2 * i + j, 0),
                        --h_bus_dpu_in_out0_1_left   => h_bus_dpu_seg_0(2 * i + j, 1),
                        h_bus_dpu_in_out0_3_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out0_4_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out0_0_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out0_1_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out0_3_left  => h_bus_dpu_seg_0(2 * i + j, 3),
                        h_bus_dpu_out_out0_4_left  => h_bus_dpu_seg_0(2 * i + j, 4),
                        --h_bus_dpu_in_out1_0_left   => h_bus_dpu_seg_1(2 * i + j, 0),
                        --h_bus_dpu_in_out1_1_left   => h_bus_dpu_seg_1(2 * i + j, 1),
                        h_bus_dpu_in_out1_3_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out1_4_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out1_0_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out1_1_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out1_3_left  => h_bus_dpu_seg_1(2 * i + j, 3),
                        h_bus_dpu_out_out1_4_left  => h_bus_dpu_seg_1(2 * i + j, 4),

                        --Vertical Busses
                        --sel_r_ext_in               
                        sel_r_ext_in_0 => sel_r_seg(i, j)(0),
                        sel_r_ext_in_1 => sel_r_seg(i, j)(1),
                        sel_r_ext_in_2 => sel_r_seg(i, j)(2),
                        sel_r_ext_in_3 => sel_r_seg(i, j)(3),
                        sel_r_ext_in_4 => sel_r_seg(i, j)(4),
                        sel_r_ext_in_5 => sel_r_seg(i, j)(5),
                        --ext_v_input_bus_in        =>    
                        ext_v_input_bus_in_0 => v_bus(i, j)(0),
                        ext_v_input_bus_in_1 => v_bus(i, j)(1),
                        ext_v_input_bus_in_2 => v_bus(i, j)(2),
                        ext_v_input_bus_in_3 => v_bus(i, j)(3),
                        ext_v_input_bus_in_4 => v_bus(i, j)(4),
                        ext_v_input_bus_in_5 => v_bus(i, j)(5),
                        --sel_r_ext_out             =>    
                        sel_r_ext_out_0 => sel_r_seg(i, (j + 1) MOD 2)(0),
                        sel_r_ext_out_1 => sel_r_seg(i, (j + 1) MOD 2)(1),
                        sel_r_ext_out_2 => sel_r_seg(i, (j + 1) MOD 2)(2),
                        sel_r_ext_out_3 => sel_r_seg(i, (j + 1) MOD 2)(3),
                        sel_r_ext_out_4 => sel_r_seg(i, (j + 1) MOD 2)(4),
                        sel_r_ext_out_5 => sel_r_seg(i, (j + 1) MOD 2)(5),
                        --ext_v_input_bus_out       =>    
                        ext_v_input_bus_out_0 => v_bus(i, (j + 1) MOD 2)(0),
                        ext_v_input_bus_out_1 => v_bus(i, (j + 1) MOD 2)(1),
                        ext_v_input_bus_out_2 => v_bus(i, (j + 1) MOD 2)(2),
                        ext_v_input_bus_out_3 => v_bus(i, (j + 1) MOD 2)(3),
                        ext_v_input_bus_out_4 => v_bus(i, (j + 1) MOD 2)(4),
                        ext_v_input_bus_out_5 => v_bus(i, (j + 1) MOD 2)(5)
                    );
            END GENERATE;

            if_drra_top : IF j = 0 AND i > 0 AND i < (COLUMNS - 1) GENERATE -- top row, non-corner case
                Silago_top_inst : ENTITY work.Silago_top
                    PORT MAP(
                        clk   => clk,
                        rst_n => rst_n,

                        -------------------------
                        -- Address signals
                        -------------------------
                        start_row => addr_valid_right(i - 1, j), --! Start signal (connected to the valid signal of the previous block in the same row) 
                        --start_col       => '0', --! Start signal (connected to the valid signal of the previous block in the same col) 
                        prevRow => row_right(i - 1, j),    --! Row address assigned to the previous cell                     
                        prevCol => col_right(i - 1, j),    --! Col address assigned to the previous cell                     
                        valid   => addr_valid_right(i, j), --! Valid signals, used to signal that the assignment of the address is complete       
                        thisRow => row_right(i, j),        --! The row address assigned to the cell                          
                        thisCol => col_right(i, j),        --! The column address assigned to the cell                       
                        ------------------------------
                        -- Data in (from next row)
                        ------------------------------
                        data_in_next               => dimarch_silego_data_out(i, j + 1), --! data from other row   
                        dimarch_silego_rd_out_next => dimarch_silego_rd_out(i, 0),       --! ready signal from the other row         

                        ------------------------------
                        -- Data out to DiMArch
                        ------------------------------
                        dimarch_data_in_out => dimarch_silego_data_in(i, j + 1), --! data from DiMArch to the next row 

                        ------------------------------
                        -- Global signals for configuration
                        ------------------------------
                        -- inputs (left hand side)
                        instr_ld       => instr_ld_right(i - 1, j),       --! load instruction signal                                                                                                     
                        instr_inp      => instr_inp_right(i - 1, j),      --! Actual instruction to be loaded                                                            
                        seq_address_rb => seq_address_rb_right(i - 1, j), --! in order to generate addresses for sequencer rows                                                 
                        seq_address_cb => seq_address_cb_right(i - 1, j), --! in order to generate addresses for sequencer cols                                              
                        -- outputs (right hand side)
                        instr_ld_out       => instr_ld_right(i, j),       --! Registered instruction load signal, broadcast to the next cell                                                              
                        instr_inp_out      => instr_inp_right(i, j),      --! Registered instruction signal, bradcast to the next cell                                   
                        seq_address_rb_out => seq_address_rb_right(i, j), --! registered signal, broadcast to the next cell, in order to generate addresses for sequencer rows  
                        seq_address_cb_out => seq_address_cb_right(i, j), --! registed signal, broadcast to the next cell, in order to generate addresses for sequencer cols 
                        ------------------------------
                        -- Silego core cell
                        ------------------------------
                        --RegFile
                        -- Data transfer only allowed through the dimarch
                        dimarch_data_in  => dimarch_silego_data_in(i, j),  --! data from dimarch (top)
                        dimarch_data_out => dimarch_silego_data_out(i, j), --! data out to dimarch (top)
                        ------------------------------
                        -- DiMArch bus output
                        ------------------------------
                        noc_bus_out => noc_bus_out(i, j), --! noc bus signal to the dimarch (top)
                        ------------------------------
                        -- NoC bus from the next row to the DiMArch
                        ------------------------------
                        -- TODO we can move the noc bus selector from the DiMArch to the cell in order to save some routing
                        noc_bus_in => noc_bus_out(i, j + 1), --! noc bus signal from the adjacent row (bottom)                          
                        ------------------------------
                        --Horizontal Busses
                        ------------------------------
                        --Horizontal Busses
                        h_bus_reg_in_out0_0_left   => h_bus_reg_seg_0(2 * i + j, 0), -- h_bus_reg_seg_0(i+1,0) ,
                        h_bus_reg_in_out0_1_left   => h_bus_reg_seg_0(2 * i + j, 1), --h_bus_reg_seg_0(i+1,1),
                        h_bus_reg_in_out0_3_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out0_4_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out0_0_right => h_bus_reg_seg_0(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out0_1_right => h_bus_reg_seg_0(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out0_3_left  => h_bus_reg_seg_0(2 * i + j, 3),
                        h_bus_reg_out_out0_4_left  => h_bus_reg_seg_0(2 * i + j, 4),
                        h_bus_reg_in_out1_0_left   => h_bus_reg_seg_1(2 * i + j, 0),
                        h_bus_reg_in_out1_1_left   => h_bus_reg_seg_1(2 * i + j, 1),
                        h_bus_reg_in_out1_3_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out1_4_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out1_0_right => h_bus_reg_seg_1(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out1_1_right => h_bus_reg_seg_1(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out1_3_left  => h_bus_reg_seg_1(2 * i + j, 3),
                        h_bus_reg_out_out1_4_left  => h_bus_reg_seg_1(2 * i + j, 4),

                        h_bus_dpu_in_out0_0_left   => h_bus_dpu_seg_0(2 * i + j, 0),
                        h_bus_dpu_in_out0_1_left   => h_bus_dpu_seg_0(2 * i + j, 1),
                        h_bus_dpu_in_out0_3_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out0_4_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out0_0_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out0_1_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out0_3_left  => h_bus_dpu_seg_0(2 * i + j, 3),
                        h_bus_dpu_out_out0_4_left  => h_bus_dpu_seg_0(2 * i + j, 4),
                        h_bus_dpu_in_out1_0_left   => h_bus_dpu_seg_1(2 * i + j, 0),
                        h_bus_dpu_in_out1_1_left   => h_bus_dpu_seg_1(2 * i + j, 1),
                        h_bus_dpu_in_out1_3_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out1_4_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out1_0_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out1_1_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out1_3_left  => h_bus_dpu_seg_1(2 * i + j, 3),
                        h_bus_dpu_out_out1_4_left  => h_bus_dpu_seg_1(2 * i + j, 4),

                        --Vertical Busses
                        --sel_r_ext_in               
                        sel_r_ext_in_0 => sel_r_seg(i, j)(0),
                        sel_r_ext_in_1 => sel_r_seg(i, j)(1),
                        sel_r_ext_in_2 => sel_r_seg(i, j)(2),
                        sel_r_ext_in_3 => sel_r_seg(i, j)(3),
                        sel_r_ext_in_4 => sel_r_seg(i, j)(4),
                        sel_r_ext_in_5 => sel_r_seg(i, j)(5),
                        --ext_v_input_bus_in        =>    
                        ext_v_input_bus_in_0 => v_bus(i, j)(0),
                        ext_v_input_bus_in_1 => v_bus(i, j)(1),
                        ext_v_input_bus_in_2 => v_bus(i, j)(2),
                        ext_v_input_bus_in_3 => v_bus(i, j)(3),
                        ext_v_input_bus_in_4 => v_bus(i, j)(4),
                        ext_v_input_bus_in_5 => v_bus(i, j)(5),
                        --sel_r_ext_out             =>    
                        sel_r_ext_out_0 => sel_r_seg(i, (j + 1) MOD 2)(0),
                        sel_r_ext_out_1 => sel_r_seg(i, (j + 1) MOD 2)(1),
                        sel_r_ext_out_2 => sel_r_seg(i, (j + 1) MOD 2)(2),
                        sel_r_ext_out_3 => sel_r_seg(i, (j + 1) MOD 2)(3),
                        sel_r_ext_out_4 => sel_r_seg(i, (j + 1) MOD 2)(4),
                        sel_r_ext_out_5 => sel_r_seg(i, (j + 1) MOD 2)(5),
                        --ext_v_input_bus_out       =>    
                        ext_v_input_bus_out_0 => v_bus(i, (j + 1) MOD 2)(0),
                        ext_v_input_bus_out_1 => v_bus(i, (j + 1) MOD 2)(1),
                        ext_v_input_bus_out_2 => v_bus(i, (j + 1) MOD 2)(2),
                        ext_v_input_bus_out_3 => v_bus(i, (j + 1) MOD 2)(3),
                        ext_v_input_bus_out_4 => v_bus(i, (j + 1) MOD 2)(4),
                        ext_v_input_bus_out_5 => v_bus(i, (j + 1) MOD 2)(5)
                    );
            END GENERATE;

            if_drra_top_r_corner : IF j = 0 AND i = COLUMNS - 1 GENERATE -- top row, corners
                Silago_top_r_corner_inst : ENTITY work.Silago_top_right_corner
                    PORT MAP(
                        clk   => clk,
                        rst_n => rst_n,

                        -------------------------
                        -- Address signals
                        -------------------------
                        start_row => addr_valid_right(i - 1, j), --! Start signal (connected to the valid signal of the previous block in the same row) 
                        --start_col       => '0', --! Start signal (connected to the valid signal of the previous block in the same col) 
                        prevRow => row_right(i - 1, j), --! Row address assigned to the previous cell                     
                        prevCol => col_right(i - 1, j), --! Col address assigned to the previous cell 
                        ------------------------------
                        -- Data in (from next row)
                        ------------------------------
                        data_in_next               => dimarch_silego_data_out(i, j + 1), --! data from other row
                        dimarch_silego_rd_out_next => dimarch_silego_rd_out(i, 0),       --! ready signal from the other row

                        ------------------------------
                        -- Data out to DiMArch
                        ------------------------------
                        dimarch_data_in_out => dimarch_silego_data_in(i, j + 1), --! data from DiMArch to the next row

                        ------------------------------
                        -- Global signals for configuration
                        ------------------------------
                        -- inputs (left hand side)
                        instr_ld       => instr_ld_right(i - 1, j),       --! load instruction signal                                                                                                     
                        instr_inp      => instr_inp_right(i - 1, j),      --! Actual instruction to be loaded                                                            
                        seq_address_rb => seq_address_rb_right(i - 1, j), --! in order to generate addresses for sequencer rows                                                 
                        seq_address_cb => seq_address_cb_right(i - 1, j), --! in order to generate addresses for sequencer cols 
                        ------------------------------
                        -- Silego core cell
                        ------------------------------
                        --RegFile
                        -- Data transfer only allowed through the dimarch
                        dimarch_data_in  => dimarch_silego_data_in(i, j),  --! data from dimarch (top)
                        dimarch_data_out => dimarch_silego_data_out(i, j), --! data out to dimarch (top)
                        ------------------------------
                        -- DiMArch bus output
                        ------------------------------
                        noc_bus_out => noc_bus_out(i, j), --! noc bus signal to the dimarch (top)
                        ------------------------------
                        -- NoC bus from the next row to the DiMArch
                        ------------------------------
                        -- TODO we can move the noc bus selector from the DiMArch to the cell in order to save some routing
                        noc_bus_in => noc_bus_out(i, j + 1), --! noc bus signal from the adjacent row (bottom)
                        ------------------------------
                        --Horizontal Busses
                        ------------------------------
                        ---------------------------------------------------------------------------------------
                        -- Modified by Dimitris to remove inputs and outputs that are not connected (left hand side)
                        -- Date 15/03/2018
                        ---------------------------------------------------------------------------------------
                        --Horizontal Busses
                        h_bus_reg_in_out0_0_left => h_bus_reg_seg_0(2 * i + j, 0),
                        h_bus_reg_in_out0_1_left => h_bus_reg_seg_0(2 * i + j, 1),
                        --h_bus_reg_in_out0_3_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 3),
                        --h_bus_reg_in_out0_4_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out0_0_right => h_bus_reg_seg_0(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out0_1_right => h_bus_reg_seg_0(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out0_3_left  => h_bus_reg_seg_0(2 * i + j, 3),
                        h_bus_reg_out_out0_4_left  => h_bus_reg_seg_0(2 * i + j, 4),

                        h_bus_reg_in_out1_0_left => h_bus_reg_seg_1(2 * i + j, 0),
                        h_bus_reg_in_out1_1_left => h_bus_reg_seg_1(2 * i + j, 1),
                        --h_bus_reg_in_out1_3_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 3),
                        --h_bus_reg_in_out1_4_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out1_0_right => h_bus_reg_seg_1(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out1_1_right => h_bus_reg_seg_1(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out1_3_left  => h_bus_reg_seg_1(2 * i + j, 3),
                        h_bus_reg_out_out1_4_left  => h_bus_reg_seg_1(2 * i + j, 4),

                        h_bus_dpu_in_out0_0_left => h_bus_dpu_seg_0(2 * i + j, 0),
                        h_bus_dpu_in_out0_1_left => h_bus_dpu_seg_0(2 * i + j, 1),
                        --h_bus_dpu_in_out0_3_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 3),
                        --h_bus_dpu_in_out0_4_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out0_0_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out0_1_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out0_3_left  => h_bus_dpu_seg_0(2 * i + j, 3),
                        h_bus_dpu_out_out0_4_left  => h_bus_dpu_seg_0(2 * i + j, 4),

                        h_bus_dpu_in_out1_0_left => h_bus_dpu_seg_1(2 * i + j, 0),
                        h_bus_dpu_in_out1_1_left => h_bus_dpu_seg_1(2 * i + j, 1),
                        --h_bus_dpu_in_out1_3_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 3),
                        --h_bus_dpu_in_out1_4_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out1_0_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out1_1_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out1_3_left  => h_bus_dpu_seg_1(2 * i + j, 3),
                        h_bus_dpu_out_out1_4_left  => h_bus_dpu_seg_1(2 * i + j, 4),

                        --Vertical Busses
                        --sel_r_ext_in               
                        sel_r_ext_in_0 => sel_r_seg(i, j)(0),
                        sel_r_ext_in_1 => sel_r_seg(i, j)(1),
                        sel_r_ext_in_2 => sel_r_seg(i, j)(2),
                        sel_r_ext_in_3 => sel_r_seg(i, j)(3),
                        sel_r_ext_in_4 => sel_r_seg(i, j)(4),
                        sel_r_ext_in_5 => sel_r_seg(i, j)(5),
                        --ext_v_input_bus_in        =>    
                        ext_v_input_bus_in_0 => v_bus(i, j)(0),
                        ext_v_input_bus_in_1 => v_bus(i, j)(1),
                        ext_v_input_bus_in_2 => v_bus(i, j)(2),
                        ext_v_input_bus_in_3 => v_bus(i, j)(3),
                        ext_v_input_bus_in_4 => v_bus(i, j)(4),
                        ext_v_input_bus_in_5 => v_bus(i, j)(5),
                        --sel_r_ext_out             =>    
                        sel_r_ext_out_0 => sel_r_seg(i, (j + 1) MOD 2)(0),
                        sel_r_ext_out_1 => sel_r_seg(i, (j + 1) MOD 2)(1),
                        sel_r_ext_out_2 => sel_r_seg(i, (j + 1) MOD 2)(2),
                        sel_r_ext_out_3 => sel_r_seg(i, (j + 1) MOD 2)(3),
                        sel_r_ext_out_4 => sel_r_seg(i, (j + 1) MOD 2)(4),
                        sel_r_ext_out_5 => sel_r_seg(i, (j + 1) MOD 2)(5),
                        --ext_v_input_bus_out       =>    
                        ext_v_input_bus_out_0 => v_bus(i, (j + 1) MOD 2)(0),
                        ext_v_input_bus_out_1 => v_bus(i, (j + 1) MOD 2)(1),
                        ext_v_input_bus_out_2 => v_bus(i, (j + 1) MOD 2)(2),
                        ext_v_input_bus_out_3 => v_bus(i, (j + 1) MOD 2)(3),
                        ext_v_input_bus_out_4 => v_bus(i, (j + 1) MOD 2)(4),
                        ext_v_input_bus_out_5 => v_bus(i, (j + 1) MOD 2)(5)
                    );
            END GENERATE;

            if_drra_bot_l_corner : IF j = (ROWS - 1) AND i = 0 GENERATE -- bottom row, corner case
                Silago_bot_l_corner_inst : ENTITY work.Silago_bot_left_corner
                    PORT MAP(
                        clk   => clk,
                        rst_n => rst_n,
                        -------------------------
                        -- Address signals
                        -------------------------
                        --start_row               => '0', --! Start signal (connected to the valid signal of the previous block in the same row)
                        start_col => addr_valid_bot(i, j - 1), --! Start signal (connected to the valid signal of the previous block in the same col)
                        prevRow   => row_bot(i, j - 1),        --! Row address assigned to the previous cell
                        prevCol   => col_bot(i, j - 1),        --! Col address assigned to the previous cell
                        valid     => addr_valid_right(i, j),   --! Valid signals, used to signal that the assignment of the address is complete
                        thisRow   => row_right(i, j),          --! The row address assigned to the cell
                        thisCol   => col_right(i, j),          --! The column address assigned to the cell
                        ------------------------------
                        -- Data in (from next row)
                        ------------------------------
                        -- TODO In this version we have removed the incoming connections from the top row, if a DiMArch is connected to the bottom row also a better scheme needs to be decided
                        --		data_in_next                 : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); --! data from other row
                        --		dimarch_silego_rd_2_out_next : in  std_logic; --! ready signal from the other row
                        ------------------------------
                        -- Data in (to next row)
                        ------------------------------
                        dimarch_rd_out   => dimarch_silego_rd_out(i, 0),   --! ready signal to the adjacent row (top)
                        dimarch_data_out => dimarch_silego_data_out(i, j), --! data out to the adjacent row (top)

                        ------------------------------
                        -- Global signals for configuration
                        ------------------------------
                        -- inputs
                        instr_ld       => instr_ld_bot(i, j - 1),       --! load instruction signal
                        instr_inp      => instr_inp_bot(i, j - 1),      --! Actual instruction to be loaded
                        seq_address_rb => seq_address_rb_bot(i, j - 1), --! in order to generate addresses for sequencer rows
                        seq_address_cb => seq_address_cb_bot(i, j - 1), --! in order to generate addresses for sequencer cols
                        -- outputs
                        instr_ld_out       => instr_ld_right(i, j),       --! Registered instruction load signal, broadcast to the next cell
                        instr_inp_out      => instr_inp_right(i, j),      --! Registered instruction signal, bradcast to the next cell
                        seq_address_rb_out => seq_address_rb_right(i, j), --! registered signal, broadcast to the next cell, in order to generate addresses for sequencer rows
                        seq_address_cb_out => seq_address_cb_right(i, j), --! registed signal, broadcast to the next cell, in order to generate addresses for sequencer cols

                        ------------------------------
                        -- Silego core cell
                        ------------------------------

                        -----------------------------
                        -- DiMArch data
                        -----------------------------
                        dimarch_data_in => dimarch_silego_data_in(i, j), --! data from dimarch (through the adjacent cell) (top)
                        -- TODO this signal has been removed in this version, if a DiMArch is connected to the bottom row also we need a better shceme
                        --		dimarch_data_out             : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); --! data out to dimarch (bot)
                        -- DiMArch bus output
                        noc_bus_out => noc_bus_out(i, j), --! NoC bus signal to the adjacent row (top), to be propagated to the DiMArch
                        -----------------------------
                        --Horizontal Busses
                        -----------------------------
                        ---------------------------------------------------------------------------------------
                        -- Modified by Dimitris to remove inputs and outputs that are not connected (left hand side)
                        -- Date 15/03/2018
                        ---------------------------------------------------------------------------------------
                        --Horizontal Busses
                        --h_bus_reg_in_out0_0_left   => h_bus_reg_seg_0(2 * i + j, 0),
                        --h_bus_reg_in_out0_1_left   => h_bus_reg_seg_0(2 * i + j, 1), 
                        h_bus_reg_in_out0_3_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out0_4_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out0_0_right => h_bus_reg_seg_0(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out0_1_right => h_bus_reg_seg_0(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out0_3_left  => h_bus_reg_seg_0(2 * i + j, 3),
                        h_bus_reg_out_out0_4_left  => h_bus_reg_seg_0(2 * i + j, 4),

                        --h_bus_reg_in_out1_0_left   => h_bus_reg_seg_1(2 * i + j, 0),
                        --h_bus_reg_in_out1_1_left   => h_bus_reg_seg_1(2 * i + j, 1),
                        h_bus_reg_in_out1_3_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out1_4_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out1_0_right => h_bus_reg_seg_1(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out1_1_right => h_bus_reg_seg_1(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out1_3_left  => h_bus_reg_seg_1(2 * i + j, 3),
                        h_bus_reg_out_out1_4_left  => h_bus_reg_seg_1(2 * i + j, 4),

                        --h_bus_dpu_in_out0_0_left   => h_bus_dpu_seg_0(2 * i + j, 0),
                        --h_bus_dpu_in_out0_1_left   => h_bus_dpu_seg_0(2 * i + j, 1),
                        h_bus_dpu_in_out0_3_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out0_4_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out0_0_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out0_1_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out0_3_left  => h_bus_dpu_seg_0(2 * i + j, 3),
                        h_bus_dpu_out_out0_4_left  => h_bus_dpu_seg_0(2 * i + j, 4),

                        --h_bus_dpu_in_out1_0_left   => h_bus_dpu_seg_1(2 * i + j, 0),
                        --h_bus_dpu_in_out1_1_left   => h_bus_dpu_seg_1(2 * i + j, 1),
                        h_bus_dpu_in_out1_3_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out1_4_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out1_0_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out1_1_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out1_3_left  => h_bus_dpu_seg_1(2 * i + j, 3),
                        h_bus_dpu_out_out1_4_left  => h_bus_dpu_seg_1(2 * i + j, 4),

                        --Vertical Busses
                        --sel_r_ext_in               
                        sel_r_ext_in_0 => sel_r_seg(i, j)(0),
                        sel_r_ext_in_1 => sel_r_seg(i, j)(1),
                        sel_r_ext_in_2 => sel_r_seg(i, j)(2),
                        sel_r_ext_in_3 => sel_r_seg(i, j)(3),
                        sel_r_ext_in_4 => sel_r_seg(i, j)(4),
                        sel_r_ext_in_5 => sel_r_seg(i, j)(5),
                        --ext_v_input_bus_in        =>    
                        ext_v_input_bus_in_0 => v_bus(i, j)(0),
                        ext_v_input_bus_in_1 => v_bus(i, j)(1),
                        ext_v_input_bus_in_2 => v_bus(i, j)(2),
                        ext_v_input_bus_in_3 => v_bus(i, j)(3),
                        ext_v_input_bus_in_4 => v_bus(i, j)(4),
                        ext_v_input_bus_in_5 => v_bus(i, j)(5),
                        --sel_r_ext_out             =>    
                        sel_r_ext_out_0 => sel_r_seg(i, (j + 1) MOD 2)(0),
                        sel_r_ext_out_1 => sel_r_seg(i, (j + 1) MOD 2)(1),
                        sel_r_ext_out_2 => sel_r_seg(i, (j + 1) MOD 2)(2),
                        sel_r_ext_out_3 => sel_r_seg(i, (j + 1) MOD 2)(3),
                        sel_r_ext_out_4 => sel_r_seg(i, (j + 1) MOD 2)(4),
                        sel_r_ext_out_5 => sel_r_seg(i, (j + 1) MOD 2)(5),
                        --ext_v_input_bus_out       =>    
                        ext_v_input_bus_out_0 => v_bus(i, (j + 1) MOD 2)(0),
                        ext_v_input_bus_out_1 => v_bus(i, (j + 1) MOD 2)(1),
                        ext_v_input_bus_out_2 => v_bus(i, (j + 1) MOD 2)(2),
                        ext_v_input_bus_out_3 => v_bus(i, (j + 1) MOD 2)(3),
                        ext_v_input_bus_out_4 => v_bus(i, (j + 1) MOD 2)(4),
                        ext_v_input_bus_out_5 => v_bus(i, (j + 1) MOD 2)(5)
                    );
            END GENERATE;

            if_drra_bot : IF j = (ROWS - 1) AND i > 0 AND i < (COLUMNS - 1) GENERATE -- bottom row, non-corner case
                Silago_bot_inst : ENTITY work.Silago_bot
                    PORT MAP(
                        clk   => clk,
                        rst_n => rst_n,

                        -------------------------
                        -- Address signals
                        -------------------------
                        start_row => addr_valid_right(i - 1, j), --! Start signal (connected to the valid signal of the previous block in the same row) 
                        --start_col       => '0', --! Start signal (connected to the valid signal of the previous block in the same col) 
                        prevRow => row_right(i - 1, j),    --! Row address assigned to the previous cell                     
                        prevCol => col_right(i - 1, j),    --! Col address assigned to the previous cell                     
                        valid   => addr_valid_right(i, j), --! Valid signals, used to signal that the assignment of the address is complete       
                        thisRow => row_right(i, j),        --! The row address assigned to the cell                          
                        thisCol => col_right(i, j),        --! The column address assigned to the cell  
                        ------------------------------
                        -- Data in (from next row)
                        ------------------------------
                        -- TODO In this version we have removed the incoming connections from the top row, if a DiMArch is connected to the bottom row also a better scheme needs to be decided
                        --		data_in_next                 : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); --! data from other row
                        --		dimarch_silego_rd_2_out_next : in  std_logic; --! ready signal from the other row
                        ------------------------------
                        -- Data in (to next row)
                        ------------------------------
                        dimarch_rd_out   => dimarch_silego_rd_out(i, 0),   --! ready signal to the adjacent row (top)
                        dimarch_data_out => dimarch_silego_data_out(i, j), --! data out to the adjacent row (top)

                        ------------------------------
                        -- Global signals for configuration
                        ------------------------------
                        -- inputs (left hand side)
                        instr_ld       => instr_ld_right(i - 1, j),       --! load instruction signal                                                                                                     
                        instr_inp      => instr_inp_right(i - 1, j),      --! Actual instruction to be loaded                                                            
                        seq_address_rb => seq_address_rb_right(i - 1, j), --! in order to generate addresses for sequencer rows                                                 
                        seq_address_cb => seq_address_cb_right(i - 1, j), --! in order to generate addresses for sequencer cols                                              
                        -- outputs (right hand side)
                        instr_ld_out       => instr_ld_right(i, j),       --! Registered instruction load signal, broadcast to the next cell                                                              
                        instr_inp_out      => instr_inp_right(i, j),      --! Registered instruction signal, bradcast to the next cell                                   
                        seq_address_rb_out => seq_address_rb_right(i, j), --! registered signal, broadcast to the next cell, in order to generate addresses for sequencer rows  
                        seq_address_cb_out => seq_address_cb_right(i, j), --! registed signal, broadcast to the next cell, in order to generate addresses for sequencer cols 
                        ------------------------------
                        -- Silego core cell
                        ------------------------------
                        --RegFile
                        -----------------------------
                        -- DiMArch data
                        -----------------------------
                        dimarch_data_in => dimarch_silego_data_in(i, j), --! data from dimarch (through the adjacent cell) (top)
                        -- TODO this signal has been removed in this version, if a DiMArch is connected to the bottom row also we need a better shceme
                        --		dimarch_data_out             : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); --! data out to dimarch (bot)
                        -- DiMArch bus output
                        noc_bus_out => noc_bus_out(i, j), --! NoC bus signal to the adjacent row (top), to be propagated to the DiMArch
                        -----------------------------
                        --Horizontal Busses
                        -----------------------------
                        --Horizontal Busses
                        h_bus_reg_in_out0_0_left   => h_bus_reg_seg_0(2 * i + j, 0), -- h_bus_reg_seg_0(i+1,0) ,
                        h_bus_reg_in_out0_1_left   => h_bus_reg_seg_0(2 * i + j, 1), --h_bus_reg_seg_0(i+1,1),
                        h_bus_reg_in_out0_3_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out0_4_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out0_0_right => h_bus_reg_seg_0(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out0_1_right => h_bus_reg_seg_0(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out0_3_left  => h_bus_reg_seg_0(2 * i + j, 3),
                        h_bus_reg_out_out0_4_left  => h_bus_reg_seg_0(2 * i + j, 4),
                        h_bus_reg_in_out1_0_left   => h_bus_reg_seg_1(2 * i + j, 0),
                        h_bus_reg_in_out1_1_left   => h_bus_reg_seg_1(2 * i + j, 1),
                        h_bus_reg_in_out1_3_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 3),
                        h_bus_reg_in_out1_4_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out1_0_right => h_bus_reg_seg_1(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out1_1_right => h_bus_reg_seg_1(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out1_3_left  => h_bus_reg_seg_1(2 * i + j, 3),
                        h_bus_reg_out_out1_4_left  => h_bus_reg_seg_1(2 * i + j, 4),

                        h_bus_dpu_in_out0_0_left   => h_bus_dpu_seg_0(2 * i + j, 0),
                        h_bus_dpu_in_out0_1_left   => h_bus_dpu_seg_0(2 * i + j, 1),
                        h_bus_dpu_in_out0_3_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out0_4_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out0_0_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out0_1_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out0_3_left  => h_bus_dpu_seg_0(2 * i + j, 3),
                        h_bus_dpu_out_out0_4_left  => h_bus_dpu_seg_0(2 * i + j, 4),
                        h_bus_dpu_in_out1_0_left   => h_bus_dpu_seg_1(2 * i + j, 0),
                        h_bus_dpu_in_out1_1_left   => h_bus_dpu_seg_1(2 * i + j, 1),
                        h_bus_dpu_in_out1_3_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 3),
                        h_bus_dpu_in_out1_4_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out1_0_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out1_1_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out1_3_left  => h_bus_dpu_seg_1(2 * i + j, 3),
                        h_bus_dpu_out_out1_4_left  => h_bus_dpu_seg_1(2 * i + j, 4),

                        --Vertical Busses
                        --sel_r_ext_in               
                        sel_r_ext_in_0 => sel_r_seg(i, j)(0),
                        sel_r_ext_in_1 => sel_r_seg(i, j)(1),
                        sel_r_ext_in_2 => sel_r_seg(i, j)(2),
                        sel_r_ext_in_3 => sel_r_seg(i, j)(3),
                        sel_r_ext_in_4 => sel_r_seg(i, j)(4),
                        sel_r_ext_in_5 => sel_r_seg(i, j)(5),
                        --ext_v_input_bus_in        =>    
                        ext_v_input_bus_in_0 => v_bus(i, j)(0),
                        ext_v_input_bus_in_1 => v_bus(i, j)(1),
                        ext_v_input_bus_in_2 => v_bus(i, j)(2),
                        ext_v_input_bus_in_3 => v_bus(i, j)(3),
                        ext_v_input_bus_in_4 => v_bus(i, j)(4),
                        ext_v_input_bus_in_5 => v_bus(i, j)(5),
                        --sel_r_ext_out             =>    
                        sel_r_ext_out_0 => sel_r_seg(i, (j + 1) MOD 2)(0),
                        sel_r_ext_out_1 => sel_r_seg(i, (j + 1) MOD 2)(1),
                        sel_r_ext_out_2 => sel_r_seg(i, (j + 1) MOD 2)(2),
                        sel_r_ext_out_3 => sel_r_seg(i, (j + 1) MOD 2)(3),
                        sel_r_ext_out_4 => sel_r_seg(i, (j + 1) MOD 2)(4),
                        sel_r_ext_out_5 => sel_r_seg(i, (j + 1) MOD 2)(5),
                        --ext_v_input_bus_out       =>    
                        ext_v_input_bus_out_0 => v_bus(i, (j + 1) MOD 2)(0),
                        ext_v_input_bus_out_1 => v_bus(i, (j + 1) MOD 2)(1),
                        ext_v_input_bus_out_2 => v_bus(i, (j + 1) MOD 2)(2),
                        ext_v_input_bus_out_3 => v_bus(i, (j + 1) MOD 2)(3),
                        ext_v_input_bus_out_4 => v_bus(i, (j + 1) MOD 2)(4),
                        ext_v_input_bus_out_5 => v_bus(i, (j + 1) MOD 2)(5)
                    );
            END GENERATE;

            if_drra_bot_r_corner : IF j = (ROWS - 1) AND i = COLUMNS - 1 GENERATE -- bottom row, corner case
                Silago_bot_r_corner_inst : ENTITY work.Silago_bot_right_corner
                    PORT MAP(
                        clk   => clk,
                        rst_n => rst_n,
                        -------------------------
                        -- Address signals
                        -------------------------
                        start_row => addr_valid_right(i - 1, j), --! Start signal (connected to the valid signal of the previous block in the same row) 
                        --start_col       => '0', --! Start signal (connected to the valid signal of the previous block in the same col) 
                        prevRow => row_right(i - 1, j), --! Row address assigned to the previous cell
                        prevCol => col_right(i - 1, j), --! Col address assigned to the previous cell
                        ------------------------------
                        -- Data in (from next row)
                        ------------------------------
                        -- TODO In this version we have removed the incoming connections from the top row, if a DiMArch is connected to the bottom row also a better scheme needs to be decided
                        --		data_in_next                 : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); --! data from other row
                        --		dimarch_silego_rd_2_out_next : in  std_logic; --! ready signal from the other row
                        ------------------------------
                        -- Data in (to next row)
                        ------------------------------
                        dimarch_rd_out   => dimarch_silego_rd_out(i, 0),   --! ready signal to the adjacent row (top)
                        dimarch_data_out => dimarch_silego_data_out(i, j), --! data out to the adjacent row (top)

                        ------------------------------
                        -- Global signals for configuration
                        ------------------------------
                        -- inputs
                        instr_ld       => instr_ld_right(i - 1, j),       --! load instruction signal                                                                                                     
                        instr_inp      => instr_inp_right(i - 1, j),      --! Actual instruction to be loaded                                                            
                        seq_address_rb => seq_address_rb_right(i - 1, j), --! in order to generate addresses for sequencer rows                                                 
                        seq_address_cb => seq_address_cb_right(i - 1, j), --! in order to generate addresses for sequencer cols 

                        ------------------------------
                        -- Silego core cell
                        ------------------------------

                        -----------------------------
                        -- DiMArch data
                        -----------------------------
                        dimarch_data_in => dimarch_silego_data_in(i, j), --! data from dimarch (through the adjacent cell) (top)
                        -- TODO this signal has been removed in this version, if a DiMArch is connected to the bottom row also we need a better shceme
                        --		dimarch_data_out             : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); --! data out to dimarch (bot)
                        -- DiMArch bus output
                        noc_bus_out => noc_bus_out(i, j), --! NoC bus signal to the adjacent row (top), to be propagated to the DiMArch
                        -----------------------------
                        --Horizontal Busses
                        -----------------------------
                        ---------------------------------------------------------------------------------------
                        -- Modified by Dimitris to remove inputs and outputs that are not connected (left hand side)
                        -- Date 15/03/2018
                        ---------------------------------------------------------------------------------------
                        --Horizontal Busses
                        h_bus_reg_in_out0_0_left => h_bus_reg_seg_0(2 * i + j, 0),
                        h_bus_reg_in_out0_1_left => h_bus_reg_seg_0(2 * i + j, 1),
                        --h_bus_reg_in_out0_3_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 3),
                        --h_bus_reg_in_out0_4_right  => h_bus_reg_seg_0(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out0_0_right => h_bus_reg_seg_0(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out0_1_right => h_bus_reg_seg_0(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out0_3_left  => h_bus_reg_seg_0(2 * i + j, 3),
                        h_bus_reg_out_out0_4_left  => h_bus_reg_seg_0(2 * i + j, 4),

                        h_bus_reg_in_out1_0_left => h_bus_reg_seg_1(2 * i + j, 0),
                        h_bus_reg_in_out1_1_left => h_bus_reg_seg_1(2 * i + j, 1),
                        --h_bus_reg_in_out1_3_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 3),
                        --h_bus_reg_in_out1_4_right  => h_bus_reg_seg_1(2 * (i + 1) + j, 4),
                        h_bus_reg_out_out1_0_right => h_bus_reg_seg_1(2 * (i + 1) + j, 0),
                        h_bus_reg_out_out1_1_right => h_bus_reg_seg_1(2 * (i + 1) + j, 1),
                        h_bus_reg_out_out1_3_left  => h_bus_reg_seg_1(2 * i + j, 3),
                        h_bus_reg_out_out1_4_left  => h_bus_reg_seg_1(2 * i + j, 4),

                        h_bus_dpu_in_out0_0_left => h_bus_dpu_seg_0(2 * i + j, 0),
                        h_bus_dpu_in_out0_1_left => h_bus_dpu_seg_0(2 * i + j, 1),
                        --h_bus_dpu_in_out0_3_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 3),
                        --h_bus_dpu_in_out0_4_right  => h_bus_dpu_seg_0(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out0_0_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out0_1_right => h_bus_dpu_seg_0(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out0_3_left  => h_bus_dpu_seg_0(2 * i + j, 3),
                        h_bus_dpu_out_out0_4_left  => h_bus_dpu_seg_0(2 * i + j, 4),

                        h_bus_dpu_in_out1_0_left => h_bus_dpu_seg_1(2 * i + j, 0),
                        h_bus_dpu_in_out1_1_left => h_bus_dpu_seg_1(2 * i + j, 1),
                        --h_bus_dpu_in_out1_3_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 3),
                        --h_bus_dpu_in_out1_4_right  => h_bus_dpu_seg_1(2 * (i + 1) + j, 4),
                        h_bus_dpu_out_out1_0_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 0),
                        h_bus_dpu_out_out1_1_right => h_bus_dpu_seg_1(2 * (i + 1) + j, 1),
                        h_bus_dpu_out_out1_3_left  => h_bus_dpu_seg_1(2 * i + j, 3),
                        h_bus_dpu_out_out1_4_left  => h_bus_dpu_seg_1(2 * i + j, 4),

                        --Vertical Busses
                        --sel_r_ext_in               
                        sel_r_ext_in_0 => sel_r_seg(i, j)(0),
                        sel_r_ext_in_1 => sel_r_seg(i, j)(1),
                        sel_r_ext_in_2 => sel_r_seg(i, j)(2),
                        sel_r_ext_in_3 => sel_r_seg(i, j)(3),
                        sel_r_ext_in_4 => sel_r_seg(i, j)(4),
                        sel_r_ext_in_5 => sel_r_seg(i, j)(5),
                        --ext_v_input_bus_in        =>    
                        ext_v_input_bus_in_0 => v_bus(i, j)(0),
                        ext_v_input_bus_in_1 => v_bus(i, j)(1),
                        ext_v_input_bus_in_2 => v_bus(i, j)(2),
                        ext_v_input_bus_in_3 => v_bus(i, j)(3),
                        ext_v_input_bus_in_4 => v_bus(i, j)(4),
                        ext_v_input_bus_in_5 => v_bus(i, j)(5),
                        --sel_r_ext_out             =>    
                        sel_r_ext_out_0 => sel_r_seg(i, (j + 1) MOD 2)(0),
                        sel_r_ext_out_1 => sel_r_seg(i, (j + 1) MOD 2)(1),
                        sel_r_ext_out_2 => sel_r_seg(i, (j + 1) MOD 2)(2),
                        sel_r_ext_out_3 => sel_r_seg(i, (j + 1) MOD 2)(3),
                        sel_r_ext_out_4 => sel_r_seg(i, (j + 1) MOD 2)(4),
                        sel_r_ext_out_5 => sel_r_seg(i, (j + 1) MOD 2)(5),
                        --ext_v_input_bus_out       =>    
                        ext_v_input_bus_out_0 => v_bus(i, (j + 1) MOD 2)(0),
                        ext_v_input_bus_out_1 => v_bus(i, (j + 1) MOD 2)(1),
                        ext_v_input_bus_out_2 => v_bus(i, (j + 1) MOD 2)(2),
                        ext_v_input_bus_out_3 => v_bus(i, (j + 1) MOD 2)(3),
                        ext_v_input_bus_out_4 => v_bus(i, (j + 1) MOD 2)(4),
                        ext_v_input_bus_out_5 => v_bus(i, (j + 1) MOD 2)(5)
                    );
            END GENERATE;

        END GENERATE MTRF_ROWS;

    END GENERATE MTRF_COLS;

END ARCHITECTURE rtl;