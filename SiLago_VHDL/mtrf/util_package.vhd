-------------------------------------------------------
--! @file
--! @brief Utility package for DRRA 
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
-- Title      : util_package
-- Project    : SiLago
-------------------------------------------------------------------------------
-- File       : util_package.vhd
-- Author     : sadiq  <sadiq@kth.se>
-- Company    : KTH
-- Created    : 2013-07-19
-- Last update: 2013-10-06
-- Platform   : SiLago
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Contact    : Stathis Dimitrios <stathis@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  	        Description
-- 2013-07-19  1.0      sadiq			        Created
-- 2014-02-25  2.0      Nasim Farahini    Modified
-- 2019-03-11  2.1      Stathis Dimitrios Add lic, and comments
-------------------------------------------------------------------------------

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
--                                                                         #
--This file is part of SiLago.                                             #
--                                                                         #
--    SiLago platform source code is distributed freely: you can           #
--    redistribute it and/or modify it under the terms of the GNU    	     #
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
LIBRARY ieee;
--! Use standard library
USE ieee.std_logic_1164.ALL;
--! Use the numeric library for signed and unsigned 
USE ieee.numeric_std.ALL;

--! @brief Utility package
--! @details This package contains some basic utility functions
--! 1) Log2, 2) max, 3) min
PACKAGE util_package IS
  FUNCTION log2(i : natural) RETURN integer;
  FUNCTION max_val(left, right : integer) RETURN integer;
  FUNCTION min_val(left, right : integer) RETURN integer;
END util_package;

PACKAGE BODY util_package IS

  FUNCTION log2(i : natural) RETURN integer IS
    VARIABLE tmp : real := real(i);
    VARIABLE ret : integer := 0;
  BEGIN
    WHILE tmp > 1.0 LOOP
      ret := ret + 1;                   --accumulates until tmp > 1
      tmp := tmp / 2.0;                   --divides current tmp by to to store
                                        --the remainder in tmp
    END LOOP;

    RETURN ret;
  END;

  FUNCTION max_val (left, right : integer)
    RETURN integer IS
  BEGIN  -- max
    IF left > right THEN
      RETURN left;
    ELSE
      RETURN right;
    END IF;
  END max_val;

  FUNCTION min_val (left, right : integer)
    RETURN integer IS
  BEGIN  -- max
    IF left < right THEN
      RETURN left;
    ELSE
      RETURN right;
    END IF;
  END min_val;

END;
