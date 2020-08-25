-------------------------------------------------------
--! @file register_file.vhd
--! @brief 
--! @details 
--! @author 
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
-- File       : register_file.vhd
-- Author     : 
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
USE ieee.std_logic_unsigned.ALL;
USE work.top_consts_types_package.ALL;

ENTITY register_file IS
    PORT
    (
        clk             : IN std_logic;
        rst_n           : IN std_logic;
        wr_addr_0       : IN std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
        wr_addr_1       : IN std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
        wr_addr_dimarch : IN std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 DOWNTO 0); -- NEW
        --wr_addr_tb              : in  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
        wr_data_ready_0       : IN std_logic;
        wr_data_ready_1       : IN std_logic;
        wr_data_ready_dimarch : IN std_logic; -- NEW
        --wr_data_ready_tb        : in  std_logic;
        rd_addr_0       : IN std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
        rd_addr_1       : IN std_logic_vector(REG_FILE_ADDR_WIDTH - 1 DOWNTO 0);
        rd_addr_dimarch : IN std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 DOWNTO 0); -- NEW
        --rd_addr_tb              : in  std_logic_vector(REG_FILE_MEM_ADDR_WIDTH - 1 downto 0);
        rd_0       : IN std_logic;
        rd_1       : IN std_logic;
        rd_dimarch : IN std_logic; -- NEW
        --rd_tb                   : in  std_logic;
        data_in_0       : IN signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_in_1       : IN signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        dimarch_data_in : IN STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0);
        --data_in_tb              : in  signed(REG_FILE_MEM_DATA_WIDTH - 1 downto 0);
        reg_outp_cntrl       : IN std_logic_vector(OUTPUT_CONTROL_VECTOR_SIZE - 1 DOWNTO 0);
        rd_dimarch_out       : OUT std_logic;
        data_out_reg_0_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_out_reg_0_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_out_reg_1_right : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        data_out_reg_1_left  : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
        dimarch_data_out     : OUT STD_LOGIC_VECTOR(REG_FILE_MEM_DATA_WIDTH - 1 DOWNTO 0)
        --data_out_tb             : out signed(REG_FILE_MEM_DATA_WIDTH - 1 downto 0) 
    );
END register_file;

ARCHITECTURE struct OF register_file IS
    -- Components
    COMPONENT register_row
        PORT
        (
            rst_n   : IN std_logic;
            clk     : IN std_logic;
            wr_enb  : IN std_logic;
            reg_in  : IN signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
            reg_out : OUT signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0));
    END COMPONENT;

    -- Types
    TYPE reg_file_type IS ARRAY (0 TO REG_FILE_DEPTH - 1) OF signed(REG_FILE_DATA_WIDTH - 1 DOWNTO 0);
    -- Signals
    SIGNAL wr_addr_enb_0, wr_enb_0, wr_addr_enb_1, wr_enb_1, wr_enb, wr_block_enb_expanded, wr_enb_final : std_logic_vector(REG_FILE_DEPTH - 1 DOWNTO 0);
    SIGNAL wr_addr_enb_dimarch, wr_block_enb_dimarch                                                     : std_logic_vector(NUM_OF_REG_BLOCKS - 1 DOWNTO 0);
    SIGNAL reg_in, reg_out, temp_data_block, temp_data_loc                                               : reg_file_type;
    SIGNAL rd_addr_tb_base_int, rd_addr_dimarch_base_int                                                 : INTEGER RANGE 0 TO (NUM_OF_REG_BLOCKS - 1) * MEM_BLOCK_SIZE;

BEGIN -- struct 

    rd_dimarch_out <= rd_dimarch;

    --------------------------------------------------------------------------------
    -- Wr_addr/block binary to grey conversion  
    --------------------------------------------------------------------------------

    -- generate write location address for ports 0,1
    gen_wr_addr_enb : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
    BEGIN
        wr_addr_enb_0(i) <= '1' WHEN unsigned(wr_addr_0) = i ELSE
        '0';
        wr_addr_enb_1(i) <= '1' WHEN unsigned(wr_addr_1) = i ELSE
        '0';
    END GENERATE gen_wr_addr_enb;

    -- generate write block address for dimarch port
    gen_wr_block_enb : FOR i IN 0 TO NUM_OF_REG_BLOCKS - 1 GENERATE
    BEGIN
        wr_addr_enb_dimarch(i) <= '1' WHEN unsigned(wr_addr_dimarch) = i ELSE
        '0';
    END GENERATE gen_wr_block_enb;

    --------------------------------------------------------------------------------
    -- Enable the input location_address/block_address
    --------------------------------------------------------------------------------
    -- generate write enable signals for ports 0,1
    gen_wr_addr_ready : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
    BEGIN
        wr_enb_0(i) <= wr_addr_enb_0(i) AND wr_data_ready_0;
        wr_enb_1(i) <= wr_addr_enb_1(i) AND wr_data_ready_1;
        wr_enb(i)   <= wr_enb_0(i) OR wr_enb_1(i);
    END GENERATE gen_wr_addr_ready;

    -- generate write enable signals for dimarch, tb ports
    gen_wr_block_ready : FOR i IN 0 TO NUM_OF_REG_BLOCKS - 1 GENERATE
    BEGIN
        wr_block_enb_dimarch(i) <= wr_addr_enb_dimarch(i) AND wr_data_ready_dimarch;
    END GENERATE gen_wr_block_ready;

    -- generate final write enable signal
    gen_final_wr_enb : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
        wr_block_enb_expanded(i) <= wr_block_enb_dimarch(i/MEM_BLOCK_SIZE);
        wr_enb_final(i)          <= wr_enb(i) OR wr_block_enb_expanded(i);
    END GENERATE gen_final_wr_enb;

    --------------------------------------------------------------------------------
    -- Generate the register file input data.
    -- Write access priority (descending order): tb_port, dimarch_port, port_0, port_1
    --------------------------------------------------------------------------------

    -- generate temporary input array from dimarch/tb ports combined
    gen_temp_data_block_i : FOR i IN 0 TO NUM_OF_REG_BLOCKS - 1 GENERATE
        gen_temp_data_block_j : FOR j IN 0 TO MEM_BLOCK_SIZE - 1 GENERATE
            temp_data_block(i * MEM_BLOCK_SIZE + j) <= signed(dimarch_data_in((j + 1) * BITWIDTH - 1 DOWNTO j * BITWIDTH)) WHEN wr_block_enb_dimarch(i) = '1' ELSE
            (OTHERS => '0');
        END GENERATE;
    END GENERATE;

    -- generate temporary input array from ports 0/1 combined
    gen_temp_data_loc_i : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
        -- If both port 0 and port 1 try to write the same RF location, priority is given to port 0
        temp_data_loc(i) <= data_in_0 WHEN wr_enb_0(i) = '1' ELSE
        data_in_1 WHEN wr_enb_1(i) = '1' ELSE
        (OTHERS => '0');
    END GENERATE;

    -- generate final reg_in combining inputs from ports 0/1 and dimarch/tb
    gen_reg_in : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
        -- If port 0 or port 1 try to write a location inside the same block as dimarch, priority is given to dimarch
        reg_in(i) <= temp_data_block(i) WHEN wr_block_enb_expanded(i) = '1' ELSE
        temp_data_loc(i) WHEN wr_enb(i) = '1' ELSE
        (OTHERS => '0');
    END GENERATE;

    --------------------------------------------------------------------------------
    -- Generate the Register File
    --------------------------------------------------------------------------------	

    -- Register File
    gen_register_file : FOR i IN 0 TO REG_FILE_DEPTH - 1 GENERATE
    BEGIN
        register_array : register_row
        PORT MAP
        (
            rst_n   => rst_n,
            clk     => clk,
            wr_enb  => wr_enb_final(i),
            reg_in  => reg_in(i),
            reg_out => reg_out(i)
        );
    END GENERATE gen_register_file;

    --------------------------------------------------------------------------------
    -- Generate Outputs
    --------------------------------------------------------------------------------

    data_out_reg_0_right <= reg_out(to_integer(unsigned(rd_addr_0))) WHEN (reg_outp_cntrl(0) = '1' AND rd_0 = '1') ELSE
        (OTHERS => '0');
    data_out_reg_1_right <= reg_out(to_integer(unsigned(rd_addr_1))) WHEN (reg_outp_cntrl(0) = '1' AND rd_1 = '1') ELSE
        (OTHERS => '0');
    data_out_reg_0_left <= reg_out(to_integer(unsigned(rd_addr_0))) WHEN (reg_outp_cntrl(1) = '1' AND rd_0 = '1') ELSE
        (OTHERS => '0');
    data_out_reg_1_left <= reg_out(to_integer(unsigned(rd_addr_1))) WHEN (reg_outp_cntrl(1) = '1' AND rd_1 = '1') ELSE
        (OTHERS => '0');

    rd_addr_dimarch_base_int <= MEM_BLOCK_SIZE * to_integer(unsigned(rd_addr_dimarch));

    gen_data_block_out : FOR i IN 0 TO MEM_BLOCK_SIZE - 1 GENERATE
        dimarch_data_out((i + 1) * BITWIDTH - 1 DOWNTO i * BITWIDTH) <= std_logic_vector(reg_out(rd_addr_dimarch_base_int + i));
    END GENERATE gen_data_block_out;

END struct;