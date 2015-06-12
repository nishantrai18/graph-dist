#!/bin/bash

awk  'BEGIN{
		line=0;
		FS=" ";
		printf  "graph gitgr {\n";
	}
	{
		printf "\t";
		printf "c"$1;
		printf "--";
		printf "c"$2;
		printf ";\n";	
	}
	END  {
		printf "}\n";
	}' < "input.gr" | dot -Tps > out.ps
