-------------------------------------------------------
--! @file silago_bot_corner.vhd
--! @brief UnitX
--! @details This is the DRRA top row tile.
--! @author Dimitrios Stathis
--! @version 1.0
--! @date 2018 01 15
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
-- Title      : UnitX
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : silago_bot_corner.vhd
-- Author     : Dimitrios Stathis
-- Company    : KTH
-- Created    : 15/01/2018
-- Last update: 15/01/2018
-- Platform   : SiLago
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Copyright (c) 2018
-------------------------------------------------------------------------------
-- Contact    : Dimitrios Stathis <stathis@kth.se>
-------------------------------------------------------------------------------

---------------------------------------------------------------------------
-- This is the DRRA top tile. This tile is the combination of all the components
-- That are needed to connect the DRRA fabric. It is stand-alone (can be harden)
-- and has dynamic addressing
-- Includes the following:
-- Core silego cell, including the switch and the mtrf cell
-- Data selector, selects which row will have access to the DiMArch that is connected to this row
-- Address assignment unit
--
-- Authors: Dimitrios Stathis : PhD student, KTH, Kista.
-- Contact: stathis@kth.se
-- Creation Date	: 15/01/2018
-- File		    	: silago_top.vhd
-- Last update      : 15/01/2018
--  
---------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;
USE work.seq_functions_package.ALL;
USE work.util_package.ALL;
USE work.top_consts_types_package.ALL;
--USE work.noc_types_n_constants.ALL;
USE work.noc_types_n_constants.NOC_BUS_TYPE;
USE work.noc_types_n_constants.DATA_IO_SIGNAL_TYPE;
USE work.noc_types_n_constants.ROW_WIDTH;
USE work.noc_types_n_constants.COL_WIDTH;

--! @brief This is the DRRA bottom tile.
--! @details This tile is the combination of all the components
--! That are needed to connect the DRRA fabric. It is stand-alone (can be harden)
--! and has dynamic addressing
--! Includes the following:
--! \verbatim
--! Address assignment unit
--! Data selector, selects which row will have access to the DiMArch that is connected to this row
--! DRRA silego cell  
--! \endverbatim
ENTITY Silago_top IS
    --	generic(
    --		ROW_WIDTH : integer := 1;         --! Addressing bits for row
    --		COL_WIDTH : integer := 5          --! Addressing bits for col
    --	);
    PORT (
        clk   : IN std_logic;
        rst_n : IN std_logic;
        -------------------------
        -- Address signals
        -------------------------
        start_row : IN std_logic; --! Start signal (connected to the valid signal of the previous block in the same row) -- Placed
        --start_col                    : in  std_logic; --! Start signal (connected to the valid signal of the previous block in the same col) -- Placed
        prevRow : IN UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);  --! Row address assigned to the previous cell                     -- Placed
        prevCol : IN UNSIGNED(COL_WIDTH - 1 DOWNTO 0);  --! Col address assigned to the previous cell                     -- Placed
        valid   : OUT std_logic;                        --! Valid signals, used to signal that the assignment of the address is complete       -- Placed
        thisRow : OUT UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! The row address assigned to the cell                          -- Placed
        thisCol : OUT UNSIGNED(COL_WIDTH - 1 DOWNTO 0); --! The column address assigned to the cell                       -- Placed
        ------------------------------
        -- Data in (from next row)
        ------------------------------
        data_in_next               : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0); --! data from other row   -- Placed
        dimarch_silego_rd_out_next : IN std_logic;                                 --! ready signal from the other row                       -- Placed

        ------------------------------
        -- Data out to DiMArch
        ------------------------------

        dimarch_data_in_out : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0); --! data from DiMArch to the next row -- Placed

        ------------------------------
        -- Global signals for configuration
        ------------------------------
        -- inputs (left hand side)
        instr_ld       : IN std_logic;                                  --! load instruction signal                                                                                                     -- Placed
        instr_inp      : IN std_logic_vector(INSTR_WIDTH - 1 DOWNTO 0); --! Actual instruction to be loaded                                                            -- Placed
        seq_address_rb : IN std_logic_vector(ROWS - 1 DOWNTO 0);        --! in order to generate addresses for sequencer rows                                                 -- Placed
        seq_address_cb : IN std_logic_vector(COLUMNS - 1 DOWNTO 0);     --! in order to generate addresses for sequencer cols                                              -- Placed
        -- outputs (right hand side)
        instr_ld_out       : OUT std_logic;                                  --! Registered instruction load signal, broadcast to the next cell                                                              -- Placed
        instr_inp_out      : OUT std_logic_vector(INSTR_WIDTH - 1 DOWNTO 0); --! Registered instruction signal, bradcast to the next cell                                   -- Placed
        seq_address_rb_out : OUT std_logic_vector(ROWS - 1 DOWNTO 0);        --! registered signal, broadcast to the next cell, in order to generate addresses for sequencer rows  -- Placed
        seq_address_cb_out : OUT std_logic_vector(COLUMNS - 1 DOWNTO 0);     --! registed signal, broadcast to the next cell, in order to generate addresses for sequencer cols -- Placed
        ------------------------------
        -- Silego core cell
        ------------------------------
        --RegFile
        -- Data transfer only allowed through the dimarch
        dimarch_data_in  : IN STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0);  --! data from dimarch (top)
        dimarch_data_out : OUT STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0); --! data out to dimarch (top)
        ------------------------------
        -- DiMArch bus output
        ------------------------------
        noc_bus_out : OUT NOC_BUS_TYPE; --! noc bus signal to the dimarch (top)
        ------------------------------
        -- NoC bus from the next row to the DiMArch
        ------------------------------
        -- TODO we can move the noc bus selector from the DiMArch to the cell in order to save some routing
        noc_bus_in : IN NOC_BUS_TYPE; --! noc bus signal from the adjacent row (bottom)                          -- Placed
        ------------------------------
        --Horizontal Busses
        ------------------------------
        h_bus_reg_in_out0_0_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_in_out0_1_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_in_out0_3_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_in_out0_4_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_out_out0_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_reg_out_out0_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_reg_out_out0_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_reg_out_out0_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_reg_in_out1_0_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_in_out1_1_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_in_out1_3_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_in_out1_4_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_reg_out_out1_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_reg_out_out1_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_reg_out_out1_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_reg_out_out1_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_in_out0_0_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_in_out0_1_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_in_out0_3_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_in_out0_4_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_out_out0_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_out_out0_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_out_out0_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_out_out0_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_in_out1_0_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_in_out1_1_left   : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_in_out1_3_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_in_out1_4_right  : IN signed(BITWIDTH - 1 DOWNTO 0);  -- Placed 
        h_bus_dpu_out_out1_0_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_out_out1_1_right : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_out_out1_3_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        h_bus_dpu_out_out1_4_left  : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed 
        --sel_r_ext_in               : IN  s_bus_switchbox_ty;
        sel_r_ext_in_0 : IN std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_in_1 : IN std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_in_2 : IN std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_in_3 : IN std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_in_4 : IN std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_in_5 : IN std_logic_vector(5 DOWNTO 0); -- Placed
        --ext_v_input_bus_in         : IN  v_bus_ty;
        ext_v_input_bus_in_0 : IN signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_in_1 : IN signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_in_2 : IN signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_in_3 : IN signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_in_4 : IN signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_in_5 : IN signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        --sel_r_ext_out              : OUT s_bus_switchbox_ty;
        sel_r_ext_out_0 : OUT std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_out_1 : OUT std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_out_2 : OUT std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_out_3 : OUT std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_out_4 : OUT std_logic_vector(5 DOWNTO 0); -- Placed
        sel_r_ext_out_5 : OUT std_logic_vector(5 DOWNTO 0); -- Placed
        --ext_v_input_bus_out        : OUT v_bus_ty
        ext_v_input_bus_out_0 : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_out_1 : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_out_2 : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_out_3 : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_out_4 : OUT signed(BITWIDTH - 1 DOWNTO 0); -- Placed
        ext_v_input_bus_out_5 : OUT signed(BITWIDTH - 1 DOWNTO 0)  -- Placed
    );
END ENTITY Silago_top;

--! @brief Structural architecture of the tile.
--! @details The structure of the module can be seen here:
--! \image html DRRA_top.png "DRRA top row cells"
--! Includes the following:
--! \verbatim
--! Address assignment unit
--! data selector  
--! SiLago core cell
--! \endverbatim
--! All configuration wires in these version are pipeline. Data load and store 
--! can only take place from and to the DiMArch, and not with outside of the fabric
ARCHITECTURE RTL OF Silago_top IS
    SIGNAL data_in_this               : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 DOWNTO 0); --! data from this cell
    SIGNAL dimarch_silego_rd_out_this : std_logic;                                 --! ready signal from this cell
    SIGNAL noc_bus_out_this           : NOC_BUS_TYPE;
    -------------------------
    -- Address signals
    -------------------------
    SIGNAL This_ROW           : UNSIGNED(ROW_WIDTH - 1 DOWNTO 0); --! The row address assigned to the cell
    SIGNAL This_COL           : UNSIGNED(COL_WIDTH - 1 DOWNTO 0); --! The column address assigned to the cell
    SIGNAL seq_address_rb_sig : std_logic;                        --! temporary signal that holds the value of the hot bit
    SIGNAL seq_address_cb_sig : std_logic;                        --! temporary signal that holds the value of the hot bit

BEGIN
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Register and transmit global signals
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    --TODO A new type of configuration is required, more efficient and through the dimarch
    register_transfer_global : PROCESS (clk, rst_n) IS
    BEGIN
        IF rst_n = '0' THEN
            instr_ld_out       <= '0';
            instr_inp_out      <= (OTHERS => '0');
            seq_address_rb_out <= (OTHERS => '0');
            seq_address_cb_out <= (OTHERS => '0');
            ELSIF rising_edge(clk) THEN
            instr_ld_out       <= instr_ld;
            instr_inp_out      <= instr_inp;
            seq_address_rb_out <= seq_address_rb;
            seq_address_cb_out <= seq_address_cb;
        END IF;
    END PROCESS register_transfer_global;

    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Data selector
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    u_data_selector : ENTITY work.data_selector
        PORT MAP(
            data_in_this                 => data_in_this,               -- data from this cell
            data_in_next                 => data_in_next,               -- data from the adjacent cell
            data_out                     => dimarch_data_out,           -- output to the DiMArch
            dimarch_silego_rd_2_out_this => dimarch_silego_rd_out_this, -- ready signal from this cell
            dimarch_silego_rd_out_next   => dimarch_silego_rd_out_next  -- ready signal for the adjacent cell
        );
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Noc Bus selector
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    u_bus_selector : ENTITY work.bus_selector
        PORT MAP(
            noc_bus_in0 => noc_bus_out_this,
            noc_bus_in1 => noc_bus_in,
            noc_bus_out => noc_bus_out
        );
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Connection to the adjacent row
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dimarch_data_pipe : PROCESS (clk, rst_n)
    BEGIN
        IF rst_n = '0' THEN
            dimarch_data_in_out <= (OTHERS => '0'); -- propagate the DiMArch data to the adjacent row
            ELSIF rising_edge(clk) THEN
            dimarch_data_in_out <= dimarch_data_in; -- propagate the DiMArch data to the adjacent row
        END IF;
    END PROCESS;
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -- Address assignment unit
    --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    u_addres_assign : ENTITY work.addr_assign(RTL)
        PORT MAP(
            clk       => clk,
            rst_n     => rst_n,
            start_row => start_row,
            start_col => '0',
            prevRow   => prevRow,
            prevCol   => prevCol,
            valid     => valid,
            thisRow   => This_ROW,
            thisCol   => This_COL
        );

    thisRow            <= This_ROW;
    thisCol            <= This_COL;
    seq_address_rb_sig <= seq_address_rb(to_integer(This_ROW));
    seq_address_cb_sig <= seq_address_cb(to_integer(This_COL));

    -- Silego core cell
    SILEGO_cell : ENTITY work.silego
        PORT MAP(
            dimarch_data_in  => dimarch_data_in,
            dimarch_data_out => data_in_this,
            dimarch_rd_2_out => dimarch_silego_rd_out_this,
            noc_bus_out      => noc_bus_out_this,
            clk              => clk,
            rst_n            => rst_n,
            instr_ld         => instr_ld,  --(i),                                                                                       
            instr_inp        => instr_inp, --instr_output(i)(OLD_INSTR_WIDTH-1 downto INSTR_WIDTH_DIFF),                  
            --seq_address 			                                                        
            seq_address_rb => seq_address_rb_sig,
            seq_address_cb => seq_address_cb_sig,

            --Horizontal Busses
            h_bus_reg_in_out0_0_left   => h_bus_reg_in_out0_0_left, -- h_bus_reg_seg_0(i+1,0) ,
            h_bus_reg_in_out0_1_left   => h_bus_reg_in_out0_1_left, --h_bus_reg_seg_0(i+1,1),
            h_bus_reg_in_out0_3_right  => h_bus_reg_in_out0_3_right,
            h_bus_reg_in_out0_4_right  => h_bus_reg_in_out0_4_right,
            h_bus_reg_out_out0_0_right => h_bus_reg_out_out0_0_right,
            h_bus_reg_out_out0_1_right => h_bus_reg_out_out0_1_right,
            h_bus_reg_out_out0_3_left  => h_bus_reg_out_out0_3_left,
            h_bus_reg_out_out0_4_left  => h_bus_reg_out_out0_4_left,
            h_bus_reg_in_out1_0_left   => h_bus_reg_in_out1_0_left,
            h_bus_reg_in_out1_1_left   => h_bus_reg_in_out1_1_left,
            h_bus_reg_in_out1_3_right  => h_bus_reg_in_out1_3_right,
            h_bus_reg_in_out1_4_right  => h_bus_reg_in_out1_4_right,
            h_bus_reg_out_out1_0_right => h_bus_reg_out_out1_0_right,
            h_bus_reg_out_out1_1_right => h_bus_reg_out_out1_1_right,
            h_bus_reg_out_out1_3_left  => h_bus_reg_out_out1_3_left,
            h_bus_reg_out_out1_4_left  => h_bus_reg_out_out1_4_left,
            h_bus_dpu_in_out0_0_left   => h_bus_dpu_in_out0_0_left,
            h_bus_dpu_in_out0_1_left   => h_bus_dpu_in_out0_1_left,
            h_bus_dpu_in_out0_3_right  => h_bus_dpu_in_out0_3_right,
            h_bus_dpu_in_out0_4_right  => h_bus_dpu_in_out0_4_right,
            h_bus_dpu_out_out0_0_right => h_bus_dpu_out_out0_0_right,
            h_bus_dpu_out_out0_1_right => h_bus_dpu_out_out0_1_right,
            h_bus_dpu_out_out0_3_left  => h_bus_dpu_out_out0_3_left,
            h_bus_dpu_out_out0_4_left  => h_bus_dpu_out_out0_4_left,
            h_bus_dpu_in_out1_0_left   => h_bus_dpu_in_out1_0_left,
            h_bus_dpu_in_out1_1_left   => h_bus_dpu_in_out1_1_left,
            h_bus_dpu_in_out1_3_right  => h_bus_dpu_in_out1_3_right,
            h_bus_dpu_in_out1_4_right  => h_bus_dpu_in_out1_4_right,
            h_bus_dpu_out_out1_0_right => h_bus_dpu_out_out1_0_right,
            h_bus_dpu_out_out1_1_right => h_bus_dpu_out_out1_1_right,
            h_bus_dpu_out_out1_3_left  => h_bus_dpu_out_out1_3_left,
            h_bus_dpu_out_out1_4_left  => h_bus_dpu_out_out1_4_left,
            --Vertical Busses
            --sel_r_ext_in               
            sel_r_ext_in_0 => sel_r_ext_in_0,
            sel_r_ext_in_1 => sel_r_ext_in_1,
            sel_r_ext_in_2 => sel_r_ext_in_2,
            sel_r_ext_in_3 => sel_r_ext_in_3,
            sel_r_ext_in_4 => sel_r_ext_in_4,
            sel_r_ext_in_5 => sel_r_ext_in_5,
            --ext_v_input_bus_in        =>    
            ext_v_input_bus_in_0 => ext_v_input_bus_in_0,
            ext_v_input_bus_in_1 => ext_v_input_bus_in_1,
            ext_v_input_bus_in_2 => ext_v_input_bus_in_2,
            ext_v_input_bus_in_3 => ext_v_input_bus_in_3,
            ext_v_input_bus_in_4 => ext_v_input_bus_in_4,
            ext_v_input_bus_in_5 => ext_v_input_bus_in_5,
            --sel_r_ext_out             =>--sel_r_ext_out,
            sel_r_ext_out_0 => sel_r_ext_out_0,
            sel_r_ext_out_1 => sel_r_ext_out_1,
            sel_r_ext_out_2 => sel_r_ext_out_2,
            sel_r_ext_out_3 => sel_r_ext_out_3,
            sel_r_ext_out_4 => sel_r_ext_out_4,
            sel_r_ext_out_5 => sel_r_ext_out_5,
            --ext_v_input_bus_out       =>--ext_v_input_bus_out,
            ext_v_input_bus_out_0 => ext_v_input_bus_out_0,
            ext_v_input_bus_out_1 => ext_v_input_bus_out_1,
            ext_v_input_bus_out_2 => ext_v_input_bus_out_2,
            ext_v_input_bus_out_3 => ext_v_input_bus_out_3,
            ext_v_input_bus_out_4 => ext_v_input_bus_out_4,
            ext_v_input_bus_out_5 => ext_v_input_bus_out_5
        );

END ARCHITECTURE RTL;