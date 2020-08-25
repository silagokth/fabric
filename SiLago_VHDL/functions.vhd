library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package functions is 
	--helper function
   function log2c (n : integer) return integer;
	
	--roundup division
	--function divUp (m : integer; n : integer) return integer;
	
	--Max of an sfixed number
	--function max_fixed (m : integer; n : integer) return sfixed;
	
	--Min of an sfixed number
	--function min_fixed (m : integer; n : integer) return sfixed;
end package;

package body functions is
	--computes ceil(log2(n))
   function log2c (n : integer) return integer is
		variable m, p : integer;
   begin
      m := 0;
      p := 1;
      for i in 0 to n loop
			if p < n then
				m := m + 1;
            p := p * 2;
        else 
				exit;
			end if;
      end loop;
      return m;
   end log2c;
	
	--returns m/n rounded upwards
--	function divUp (m : integer; n : integer) return integer is
--	begin
--		if (m mod n /= 0) then
--			return ((m/n)+1);
--		else
--			return (m/n);
--		end if;
--	end divUp;
--	
--	--computes the maximum sfixed number for these bounds
--	function max_fixed (m : integer; n : integer) return sfixed is
--		variable temp : sfixed(m-1 downto -n);
--	begin 
--		temp(m-1) := '0';
--		temp(m-2 downto -n) := (others => '1');
--		return temp;
--	end max_fixed;
--	
--	--computes the minimum sfixed number for these bounds
--	function min_fixed (m : integer; n : integer) return sfixed is
--		variable temp : sfixed(m-1 downto -n);
--	begin 
--		temp(m-1) := '1';
--		temp(m-2 downto -n) := (others => '0');
--		return temp;
--	end min_fixed;
	 
end functions;
