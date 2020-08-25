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
-- Project    : MTRF Fabric
-- Supervisor : Nasim Farahini
-------------------------------------------------------------------------------
-- File       : util_package.vhd
-- Author     : sadiq  <sadiq@kth.se>
-- Company    : KTH
-- Created    : 2013-07-19
-- Last update: 2013-10-06
-- Platform   : 
-- Standard   : VHDL'87
-------------------------------------------------------------------------------
-- Description: <cursor>
-------------------------------------------------------------------------------
-- Copyright (c) 2013 
-------------------------------------------------------------------------------
-- Contact    : Nasim Farahini <farahini@kth.se>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  		Description
-- 2013-07-19  1.0      sadiq			Created
-- 2014-02-25  2.0      Nasim Farahini  Modified
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

PACKAGE util_package IS
    FUNCTION log2(i              : NATURAL) RETURN INTEGER;
    FUNCTION max_val(left, right : INTEGER) RETURN INTEGER;
    FUNCTION min_val(left, right : INTEGER) RETURN INTEGER;
END util_package;

PACKAGE BODY util_package IS

    FUNCTION log2(i : NATURAL) RETURN INTEGER IS
        VARIABLE tmp    : real    := real(i);
        VARIABLE ret    : INTEGER := 0;
    BEGIN
        WHILE tmp > 1.0 LOOP
            ret := ret + 1;   --accumulates until tmp > 1
            tmp := tmp / 2.0; --divides current tmp by to to store
            --the remainder in tmp
        END LOOP;

        RETURN ret;
    END;

    FUNCTION max_val (left, right : INTEGER)
        RETURN INTEGER IS
    BEGIN -- max
        IF left > right THEN
            RETURN left;
        ELSE
            RETURN right;
        END IF;
    END max_val;

    FUNCTION min_val (left, right : INTEGER)
        RETURN INTEGER IS
    BEGIN -- max
        IF left < right THEN
            RETURN left;
        ELSE
            RETURN right;
        END IF;
    END min_val;

END;