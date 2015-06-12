#!/bin/bash

#Works for small graphs in DIMACS format

awk  'BEGIN{
		line=0;
		FS=" ";
		printf  "digraph gitgr {\n";
	}
	{
		if(line<10000)
		{
			if($1=="a")
			{
				printf "\t";
				printf "c"$2" ";
				printf "->";
				printf " c"$3;
				printf ";\n";	
			}
		}
		line++;
	}
	END  {
		printf "}\n";
	}' < "input.gr" | dot -Tps > out.ps
