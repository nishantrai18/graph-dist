#!/bin/bash

awk 'BEGIN{
		line=0;
		FS=" ";
		printf  "digraph gitgr {\n";
	}
	{
		printf "\t";
		printf "c"$1;
		printf "->";
		printf "c"$2" ";
		printf "[label=\" \", texlbl=\""$3"\"]";
		printf ";\n";	
	}
	END  {
		printf "}\n";
	}' < "input.gr" | dot2tex --crop -tmath > out.tex 

pdflatex out.tex
rm out.tex out.aux out.log
