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
-- Title      : SRAM Tile
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : SRAMTile.vhd
-- Author     : Muhammad Adeel Tajammul <tajammul@kth.se>
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
--             1.0      Muhammad Adeel Tajammul   Created
--             1.5                                Segmented bus is replaced with buffers
-- 2019-03-11  2.0      Dimitrios Stathis         Update SRAM module
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

--! IEEE Linrary
LIBRARY IEEE;
--! Use standard library
USE IEEE.std_logic_1164.ALL;
--! Use numeric standard library for arithmetic operations
USE ieee.numeric_std.ALL;
--! Use the top package with the constants and type definitions
use work.top_consts_types_package.all;
--! Use the noc type and constant package
use work.noc_types_n_constants.all;
--! Use the CORSSBAR_INSTRUCTION_RECORD_TYPE from the crossbar package
use work.crossbar_types_n_constants.CORSSBAR_INSTRUCTION_RECORD_TYPE;
--! Use the nr_of_crossbar_ports from the crossbar package
use work.crossbar_types_n_constants.nr_of_crossbar_ports;
--! Use the misc package
USE work.misc.ALL;


--! @breif This is a STile which is the second layer of dimarch.
--! @detail It does not have its own sequencer but it recieves instruction for DRRA (previously from contile) sequencer
entity STile is
	GENERIC(
		This_ROW : UNSIGNED(ROW_WIDTH - 1 DOWNTO 0) := (OTHERS => '0');
		This_COL : UNSIGNED(COL_WIDTH - 1 DOWNTO 0) := (OTHERS => '0'));
	port(
		rst_n                    : IN  STD_LOGIC;
		clk                      : IN  STD_LOGIC;
		-------------------------
		-- crossbar data signals
		-------------------------
		north_in                 : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		south_in                 : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		east_in                  : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		west_in                  : in  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		north_out                : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		south_out                : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		east_out                 : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		west_out                 : out STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		--------------------------------------------------
		-- DIRECTION OF Neibouring buses
		--------------------------------------------------
		north_splitter_direction : in  std_logic_vector(1 DOWNTO 0);
		south_splitter_direction : in  std_logic_vector(1 DOWNTO 0);
		east_splitter_direction  : in  std_logic_vector(1 DOWNTO 0);
		west_splitter_direction  : in  std_logic_vector(1 DOWNTO 0);
		--------------------------------------------------
		-- partitioning instruction to neibouring buses
		--------------------------------------------------
		top_instruction_out      : out PARTITION_INSTRUCTION_RECORD_TYPE;
		left_instruction_out     : out PARTITION_INSTRUCTION_RECORD_TYPE;
		bottom_instruction_out   : out PARTITION_INSTRUCTION_RECORD_TYPE;
		right_instruction_out    : out PARTITION_INSTRUCTION_RECORD_TYPE;
		----------------------------
		--	SEMENTED BUS I/0  
		----------------------------
		HOR_BUS_LEFT_IN          : in  NOC_BUS_TYPE;
		HOR_BUS_RIGHT_IN         : in  NOC_BUS_TYPE;
		HOR_BUS_LEFT_OUT         : out NOC_BUS_TYPE;
		HOR_BUS_RIGHT_OUT        : out NOC_BUS_TYPE;
		VER_BUS_TOP_IN           : in  NOC_BUS_TYPE;
		VER_BUS_BOTTOM_IN        : in  NOC_BUS_TYPE;
		VER_BUS_TOP_OUT          : out NOC_BUS_TYPE;
		VER_BUS_BOTTOM_OUT       : out NOC_BUS_TYPE;
		--inputs
		--outputs
		--------------------------------------------------------
		--SRAM initialization from testbench
		--------------------------------------------------------
		tb_en                    : IN  STD_LOGIC; -- Write Enable from test bench
		tb_addrs                 : IN  STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 downto 0); -- Write Address from test bench
		tb_inp                   : IN  STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);
		tb_ROW                   : IN  UNSIGNED(ROW_WIDTH - 1 DOWNTO 0);
		tb_COL                   : IN  UNSIGNED(COL_WIDTH - 1 DOWNTO 0)
	);                                  -- Make a new package file and define a constant for this signal
end STile;
--! @brief Architecture of the STile
--! @detail
ARCHITECTURE behv_rtl OF STile IS

	-----------------------------------
	-- SRAM SIGNALS 
	-----------------------------------
	signal memory_in, memory_out : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0);

	-----------------------------------
	-- SRAM AGU SIGNALs 
	-----------------------------------

	--signal        SRAM_AGU_instruction_r           , SRAM_AGU_instruction_w				: UNSIGNED (SRAM_AGU_INSTR_WIDTH-1 DOWNTO 0);
	--Bits to turn output on/off during middle delay or repetition delay
	--signal	       SRAM_AGU_valid_bits_r           , SRAM_AGU_valid_bits_w				: UNSIGNED (1 downto 0);--   -- MSB for middle delay valid, LSB for repetition delay valid
	--Signal to start processing the instruction
	--signal        SRAM_instr_start_r               , SRAM_instr_start_w              	: STD_LOGIC;
	--Signal to tell that instruction has been completed
	--signal        SRAM_instr_complete_r            , SRAM_instr_complete_w             	: STD_LOGIC;
	signal SRAM_rw_r, SRAM_rw_w --			  SRAM_inout_select_r              , SRAM_inout_select_w                
	: STD_LOGIC;
	signal SRAM_rw_addrs_out_r, SRAM_rw_addrs_out_w                                                           : UNSIGNED(SRAM_ADDRESS_WIDTH - 1 DOWNTO 0); --log2_ceil(SRAM_DEPTH)
	--signal        SRAM_toplevel_outputcontrol_r    , SRAM_toplevel_outputcontrol_w  	: UNSIGNED (1 downto 0);

	signal DIRECTION_VER : CORSSBAR_INSTRUCTION_RECORD_TYPE;
	--	signal DIRECTION_HOR: CORSSBAR_INSTRUCTION_RECORD_TYPE;
	signal AGU_en_r      : std_logic;
	signal AGU_en_w      : std_logic;
	--	signal instr_r,instr_w : sram_agu_type;
	signal reorder_r     : std_logic;
	signal reorder_w     : std_logic;
	signal instr_w       : sram_agu_instruction_type;
	signal instr_r       : sram_agu_instruction_type;
	signal inp_tmp   : STD_LOGIC_VECTOR(SRAM_WIDTH - 1 downto 0); --! Input
    signal addrs_tmp : STD_LOGIC_VECTOR(SRAM_ADDRESS_WIDTH - 1 downto 0); -- Write Address from test bench
    signal en_w_tmp  : std_logic;
BEGIN
	--------------------------
	-- instruction network 
	-------------------------

	U_ISWITCH : entity work.iSwitch(RTL)
		generic map(
			This_ROW => This_ROW,
			This_COL => This_COL
			--		vertical => 0
		)
		port map(
			rst_n                    => rst_n,
			clk                      => clk,
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
			--		HOR_BUS_OUT              => HOR_BUS_OUT,

			--		VER_BUS_OUT              => VER_BUS_OUT,

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
	SRAM_input : process(tb_COL, tb_ROW, This_COL, This_ROW, tb_en, tb_inp, memory_out, tb_addrs, SRAM_rw_addrs_out_w, SRAM_rw_w)
	begin
        if tb_en = '1' and tb_COL = This_COL and tb_ROW = This_ROW then
			inp_tmp   <= tb_inp;
            addrs_tmp <= tb_addrs;
            en_w_tmp  <= tb_en;
		else
			inp_tmp  <= memory_out;
            addrs_tmp <= std_logic_vector(SRAM_rw_addrs_out_w);
            en_w_tmp <= SRAM_rw_w;
		end if;
    end process SRAM_input;
	u_sram : SRAM
--		generic map(
--			ROW_bit => ROW_bit,
--			COL_bit => COL_bit
--		)
		port map(
			--rst_n    => rst_n,
			--clk      => clk,
			-- address 
			--This_ROW                 => This_ROW,
			--This_COL                 => This_COL,
			----------
			en_w     => en_w_tmp,
			addrs_w  => addrs_tmp,
			inp      => inp_tmp,
			en_r     => SRAM_rw_r,
			addrs_r  => std_logic_vector(SRAM_rw_addrs_out_r),
			outp     => memory_in
			--tb_en    => tb_en,          -- Write Enable from test bench
			--tb_addrs => tb_addrs,       -- Write Address from test bench
			--tb_inp   => tb_inp,         -- Input from test bench
			--tb_ROW   => tb_ROW,
			--tb_COL   => tb_COL
		);

	u_crossbar : entity work.data_crossbar(RTL)
		generic map(
			NumberofPorts => nr_of_crossbar_ports
		)
		port map(
			rst_n          => rst_n,
			clk            => clk,
			DIRECTION      => DIRECTION_VER,
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

	U_SRAM_AGU_R : entity work.sram_agu
		port map(
			rst_n        => rst_n,
			clk          => clk,
			instr        => instr_r,
			rw           => SRAM_rw_r,
			reorder      => reorder_r,
			rw_addrs_out => SRAM_rw_addrs_out_r
		);

	U_SRAM_AGU_w : entity work.sram_agu
		port map(
			rst_n        => rst_n,
			clk          => clk,
			instr        => instr_w,
			rw           => SRAM_rw_w,
			reorder      => reorder_w,
			rw_addrs_out => SRAM_rw_addrs_out_w
		);

		--	u_SRAM_AGU_r : entity work.SINGLEPORT_SRAM_AGU
		--    PORT MAP(
		----        agu_instruction_in => agu_instruction_in,
		--        rst_n                    => rst_n,
		--        clk                      => clk,
		--        instr => instr_r,
		--        
		----        AGU_instruction_in       => SRAM_AGU_instruction_r,
		--        agu_en_in                => AGU_en_r,
		--        --Bits to turn output on/off during middle delay or repetition delay
		--        --Signal to start processing the instruction
		--  --      instr_start              => SRAM_instr_start_r,
		--        --Signal to tell that instruction has been completed
		----        instr_complete           => SRAM_instr_complete_r,
		--        rw                       => SRAM_rw_r,
		--        rw_addrs_out             => SRAM_rw_addrs_out_r
		--       -- rd_wr                           =>SRAM_rd_wr_r,
		----        SRAM_inout_select        => SRAM_inout_select_r,
		----        toplevel_outputcontrol   => SRAM_toplevel_outputcontrol_r
		--	);

		--    u_SRAM_AGU_w : entity work.SINGLEPORT_SRAM_AGU
		--    PORT MAP(
		--        rst_n                    => rst_n,
		--        clk                      => clk,
		----        AGU_instruction_in       => SRAM_AGU_instruction_w,
		--         instr => instr_w,
		--        agu_en_in                => AGU_en_w,
		--        -- Signal to start processing the instruction
		-- --       instr_start              => SRAM_instr_start_w,
		--        -- Signal to tell that instruction has been completed
		----        instr_complete           => SRAM_instr_complete_w,
		--        rw                       => SRAM_rw_w,
		--        rw_addrs_out             => SRAM_rw_addrs_out_w
		----        SRAM_inout_select        => SRAM_inout_select_w,
		----        toplevel_outputcontrol   => SRAM_toplevel_outputcontrol_w
		--        );

END ARCHITECTURE;
