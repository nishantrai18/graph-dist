main: unit test

unit:
	ocamlbuild -tag debug -I src -lib bigarray src/unit.native --


test: native
	./main.native graph -file _data/USA-road-d.NY.gr.gz -format dimacs diameter_scc -node 1

native:
	ocamlbuild -tag debug -I src -lib bigarray -package camlzip -package xmlm src/main.native

osm:
	ocamlbuild -tag debug -I src -lib bigarray -lib unix -package xmlm src/osm.native

binaries: native osm

bigtest: main.native
	./main.native ../road/USA-road-t.USA.gr.gz 1 23947347

clean:
	rm -f *~ src/*~
	ocamlbuild -clean


ten:
	./main.native USA-road-t.NY.gr.gz 1 10

# ---------------- _data -------------------------

# snap
%.txt.gz.diam: native
	./main.native -random 2 graph -file $*.txt.gz -format edges diameter_scc -node random

# snap to symmetrize
%.txt.gz.symdiam: native
	./main.native -random 2 graph -file $*.txt.gz -format snap symmetrize diameter_scc -node random

# dimacs
%.gr.gz.diam: native
	./main.native read dimacs $*.gr.gz diameter

_res/%.gr.gz.scc_sizes: _data/%.gr.gz native
	./main.native read dimacs $< scc_sizes > $@

# Snap format from Dimacs format
%.snap.txt: %.gr.gz
	gunzip -c $< | grep '^a' | sed -e 's/^a //' | cut -d ' ' -f 1,2 | tr " " "\011" > $@

# -------------------- Open street map to dimacs ------------

%.osm.dimacs:
	make $*.osm.co.gz $*.osm-d.gr.gz $*.osm-t.gr.gz

_data/%.osm.co.gz: _res/%.osm.dimacs_all.gz
	gunzip -c $< | grep -e '^v ' | tr -d "." | gzip -c > $@

_data/%.osm-d.gr.gz: _res/%.osm.dimacs_all.gz
	gunzip -c $< | grep -e '^a ' | awk '{ print $$1, $$2, $$3, $$4 ; }' | gzip -c > $@

_data/%.osm-t.gr.gz: _res/%.osm.dimacs_all.gz
	gunzip -c $< | grep -e '^a ' | awk '{ print $$1, $$2, $$3, $$5 ; }' | gzip -c > $@

_res/%.osm.dimacs_all.gz: _data/%.osm.bz2
	make osm
	mkdir -p _res
	./osm.native $< | gzip -c > $@


WORLD=africa antarctica asia australia-oceania central-america europe north-america south-america

world_bz2: $(patsubst %,_data/%-latest.osm.bz2,$(WORLD))

_data/%-latest.osm.bz2:
	curl -o $@ http://download.geofabrik.de/$*-latest.osm.bz2

world_dimacs: 
	make $(patsubst %,_data/%-latest.osm.dimacs.rioc,$(WORLD))

world_diam: 
	make $(patsubst %,_data/%-latest.osm-t.gr.gz.diam.rioc,$(WORLD))

_data/world-latest.osm.co.gz: $(patsubst %,_data/%-latest.osm.co.gz,$(WORLD))
	gunzip -c $^ | gzip -c > $@

_data/world-latest.osm-t.gr.gz: $(patsubst %,_data/%-latest.osm-t.gr.gz,$(WORLD))
	gunzip -c $^ | gzip -c > $@

_data/world-latest.osm-d.gr.gz: $(patsubst %,_data/%-latest.osm-d.gr.gz,$(WORLD))
	gunzip -c $^ | gzip -c > $@

world:
	make _data/world-latest.osm.co.gz _data/world-latest.osm-t.gr.gz _data/world-latest.osm-d.gr.gz

# -------------------- Dimacs to cartodb

%.csv: %.co.csv.gz %.gr.csv.gz %.diam.csv.gz

%.co.csv.gz: %.co.gz
	export LANG='C' ; echo "Node,Longitude,Latitude" ; gunzip -c $< | grep -e '^v ' | awk '{ printf "%s,%.6f,%.6f\n", $$2, $$3/1000000.0, $$4/1000000.0 }' | gzip -c > $@

%.gr.csv.gz: %.gr.gz
	(echo "#Source,Destination" ; gunzip -c $< | grep -e '^a ' | awk '{ printf "%s,%s\n", $$2, $$3 }') | gzip -c > $@

%.diam.csv.gz: %.gr.gz
	make native
	(echo "Node,Longitude,Latitude" ; ./main.native graph -file $< -format dimacs diam_path) | gzip -c > $@

_res/%.center_tree.csv.gz: _data/%.gr.gz
	make native
	(echo "Longitude,Latitude,Depth" ; ./main.native graph -file $< -format dimacs center_tree | awk -F ',' 'BEGIN { OFS="," ; } { print $$2, $$3, $$5 ; }') | gzip -c > $@

%.center_tree.csv.gz: %.stdout
	(echo "Longitude,Latitude,Depth" ; grep -e '^[-.,0-9]*$$' $< | awk -F ',' 'BEGIN { OFS="," ; } { print $$2, $$3, $$5 ; }') | gzip -c > $@

%.diam.csv.gz: %.stdout
	(echo "Longitude,Latitude" ; grep -e '^[-.,0-9]*$$' $< | awk -F ',' 'BEGIN { OFS="," ; } { print $$2, $$3 ; }') | gzip -c > $@

www/_data/%-diam.js:
	lastout=`ls _res/OAR.*.stdout | cut -d'.' -f 2 | sort -n | tail -1`; \
	ficname="_res/OAR.$${lastout}.stdout"; \
	varname=`echo $* | tr ".-" "__"`_diam; \
	echo $$varname $$ficname ;\
	(echo "var $$varname = [" ; grep -e '^[-.,0-9]*$$' $$ficname | awk -F ',' 'BEGIN { prefix=""; } { printf "%s[%s,%s]", prefix, $$3, $$2 ; prefix=",\n"; }' ; echo "\n];") > $@

_res/%.diam.csv.gz: www/_data/%.diam.js
	mkdir -p _res
	(echo "Longitude,Latitude" ; \
	cat $< | tr "]" "\012" | sed -e 's/.*\[//' | awk -F ',' 'BEGIN { OFS="," ; } { if (NF > 1) print $$2, $$1 ; }' \
	) | gzip -c > $@

# --------------------- Dimacs to Leaflet/Mapbox ----------------

uucut=$(word $(1),$(subst __, ,$*))

www/_data/%.diam.js: _data/%.gr.gz
	make native
	varname=`echo $* | tr ".-" "__"`_diam; \
	(echo "var $$varname = " ; ./main.native graph -file $< -format dimacs diam_path ; echo " ;") > $@

www/_data/%.otree.js:
	make native
	graph="_data/$(call uucut,1).gr.gz";\
	base="`basename $(call uucut,1) -t`";\
	base="`basename $$base -d`";\
	coord="_data/$$base.co.gz";\
	node="$(call uucut,2)";\
	varname=`echo $* | tr ".-" "__"`_otree; \
	(echo "var $$varname = [" ; ./main.native read dimacs $$graph tree $$node $$coord ; echo "] ;") > $@

www/_data/%.itree.js:
	make native
	graph="_data/$(call uucut,1).gr.gz";\
	base="`basename $(call uucut,1) -t`";\
	base="`basename $$base -d`";\
	coord="_data/$$base.co.gz";\
	node="$(call uucut,2)";\
	varname=`echo $* | tr ".-" "__"`_itree; \
	(echo "var $$varname = [" ; ./main.native read dimacs $$graph reverse tree $$node $$coord ; echo "] ;") > $@

www/_data/%.diam.js:
	make native
	graph="_data/$*";\
	run=`echo $${graph}.diam | tr "/" "_"` ;\
	node=`grep 'diam :' ../$$run/OAR.*.stderr | cut -d '(' -f 2 | cut -d ')' -f 1` ;\
	base="`basename $* -t.gr.gz`";\
	base="`basename $$base -d.gr.gz`";\
	coord="_data/$$base.co.gz";\
	varname=`echo $* | tr ".-" "__"`_diam; \
	(echo -n "var $$varname = " ; ./main.native read dimacs $$graph diam_path $$node $$coord ; echo " ;") > $@

www/_data/%.mdiam.js:
	make native
	graph="_data/$*";\
	node='1814194793' ;\
	base="`basename $* -t.gr.gz`";\
	base="`basename $$base -d.gr.gz`";\
	coord="_data/$$base.co.gz";\
	varname=`echo $* | tr ".-" "__"`_diam; \
	(echo -n "var $$varname = " ; ./main.native read dimacs $$graph diam_path $$node $$coord ; echo " ;") > $@

www/_data/%.diameters.js:
	make native
	graph="_data/$*";\
	base="`basename $* -t.gr.gz`";\
	base="`basename $$base -d.gr.gz`";\
	coord="_data/$$base.co.gz";\
	varname=`echo $* | tr ".-" "__"`_diameters; \
	(echo "var $$varname = [" ; ./main.native read dimacs $$graph diameters $$coord 100 ; echo "] ;") > $@

%.closest: native
	coord="_data/$(call uucut,1).co.gz";\
	lat="$(call uucut,2)";\
	lon="$(call uucut,2)";\
	./main.native closest $$lat $$lon $$coord

%.where: native
	coord="_data/$(call uucut,1).co.gz";\
	node="$(call uucut,2)";\
	./main.native where $$node $$coord

# 257923231


# --------------------- Dimacs to graphviz dot ------------------

%.co.gz.pos: %.co.gz
	gunzip -c $< | grep -e '^v ' | awk '{ printf "%s [pos=\"%s,%s\"];\n", $$2, $$3, $$4 }'

%.gr.gz.arc: %.gr.gz
		gunzip -c $< | grep -e '^a ' | awk '{ printf "%s -> %s;\n", $$2, $$3 }'

../road/%.dot: ../road/%.co.gz ../road/%.gr.gz
	echo "digraph $* {" > $@
	gunzip -c ../road/$*.co.gz | grep -e '^v ' | awk '{ printf "%s [pos=\"%s,%s\"];\n", $$2, $$3, $$4 }' >> $@
	gunzip -c ../road/$*.gr.gz | grep -e '^a ' | awk '{ printf "%s -> %s;\n", $$2, $$3 }' >> $@
	echo "}" >> $@

# ---------------- basic oar -------------------

%.oar:
	mkdir -p _exec
	cd _exec ;\
	run=`echo $* | tr "/" "_"` ;\
	if [ -d "$$run" ] ; then \
		echo "File $$run already present !!!!!!!!!!!!!!" ;\
		exit 2 ;\
	else \
		mkdir -p $$run ;\
		cd $$run ;\
		for f in ../../* ; do ln -sf $$f ./ ; done ;\
		echo "#!/bin/bash" > $$run.sh ;\
		echo "" >> $$run.sh ;\
		echo "make $*" >> $$run.sh ;\
		chmod 755 $$run.sh ;\
		oarsub -l core=1,walltime=24:00:00 ./$$run.sh | tee $(TMP) ;\
		grep OAR_JOB_ID  $(TMP) | sed -e 's/.*=//' > jobid_OAR ;\
		rm -f $(TMP) ;\
	fi

%.rioc: rioc_scp
	mkdir -p _rioc
	run=`echo $* | tr "/" "_"` ;\
	if [ -d "_rioc/$$run" ] ; then \
		echo "File $$run already present !!!!!!!!!!!!!!" ;\
		exit 2 ;\
	else \
		ssh rioc.inria.fr "cd graph ; make binaries ; make $*.oar" | tee $(TMP) ;\
		grep OAR_JOB_ID  $(TMP) | sed -e 's/.*=//' > _rioc/$$run ;\
		rm -f $(TMP) ;\
	fi

%.prog:
	run=`echo $* | tr "/" "_"` ;\
	ssh rioc.inria.fr "cd graph/_exec ; tail -n 100 $$run/OAR.*.stdout ; echo "\n--------- err --------------\n" ; tail -n 100 -f $$run/OAR.*.stderr"

%.get:
	scp rioc.inria.fr:graph/$* `dirname $*`/

oar_stop:
	oardel `cat jobid_OAR`

%.stop:
	run=`echo $* | tr "/" "_"` ;\
	ssh rioc.inria.fr "cd graph/_exec/$$run ; make oar_stop"

%.del:
	run=`echo $* | tr "/" "_"` ;\
	ssh rioc.inria.fr "cd graph/_exec/$$run ; make oar_stop ; rm -f $* ; cd .. ; rm -fr $$run"

# ---------------- rioc.inria.fr -------------------------

TMP:=$(shell mkdir -p /tmp/viennot && mktemp -u /tmp/viennot/cluster.XXXX)

rioc_one: rioc_scp
	mkdir -p _rioc
	ssh rioc.inria.fr "cd graph ; make native ; oar/run_base.sh oar core=1,walltime=24:00:00 res_job ./main.native read dimacs _data/europe-2015-02-04.osm-t.gr.gz diameter" | tee $(TMP)
	grep OAR_JOB_ID  $(TMP) | sed -e 's/.*=//' > _rioc/last_jobid 
	(date ; cat $(TMP) ; echo) >> _rioc/submitted
	scp _rioc/last_jobid rioc.inria.fr:graph/_exec/OAR_job_id
	rm -f $(TMP)

rioc_one2: rioc_scp
	mkdir -p _rioc
	ssh rioc.inria.fr "cd graph ; make osm ; oar/run_base.sh oar core=1,walltime=24:00:00 res_job make _data/europe-2015-02-04.osm.dimacs" | tee $(TMP)
	grep OAR_JOB_ID  $(TMP) | sed -e 's/.*=//' > _rioc/last_jobid 
	(date ; cat $(TMP) ; echo) >> _rioc/submitted
	scp _rioc/last_jobid rioc.inria.fr:graph/_exec/OAR_job_id
	rm -f $(TMP)

rioc_one_prog:
	job=$$(< _rioc/last_jobid) ;\
	echo $$job ;\
	ssh rioc.inria.fr "cd graph/_exec ; tail -n 100 -f OAR.$${job}.stderr"

rioc_one_get:
	job=$$(< _rioc/last_jobid) ;\
	echo $$job ;\
	mkdir - _res ;\
	scp "rioc.inria.fr:graph/_exec/OAR.$${job}.*" _res/

%.rioc_one_center:
	job=$$(< _rioc/last_jobid) ;\
	echo $$job ;\
	(echo "Longitude,Latitude,Depth" ; grep -e '^[0-9,.]*$$' _res/OAR.$${job}.stdout | awk -F ',' '{ printf "%s,%s,%d\n", $$2, $$3, ($$5 - $$4) }') | gzip -c > _res/$*.csv.gz

%.rioc_start: rioc_scp
	mkdir -p _rioc
	ssh rioc.inria.fr "cd graph ; make native ; oar/run_base.sh oar core=$*,walltime=2:00:00 spawn_all_par ./main.native graph -file _data/USA-road-d.NY.gr.gz -format dimacs dijkstra_bulk -core_i_n " | tee $(TMP)
	grep OAR_JOB_ID  $(TMP) | sed -e 's/.*=//' > _rioc/last_jobid 
	(date ; cat $(TMP) ; echo) >> _rioc/submitted
	scp _rioc/last_jobid rioc.inria.fr:graph/_exec/OAR_job_id
	rm -f $(TMP)

%.rioc_clean: rioc_scp
	mkdir -p _rioc
	ssh rioc.inria.fr "cd graph ; make native ; oar/run_base.sh oar nodes=$*/core=1,walltime=0:10:00 spawn_all_par script_com clean_all" | tee $(TMP)
	grep OAR_JOB_ID  $(TMP) | sed -e 's/.*=//' > _rioc/last_jobid 
	(date ; cat $(TMP) ; echo) >> _rioc/submitted
	scp _rioc/last_jobid rioc.inria.fr:graph/_exec/OAR_job_id
	rm -f $(TMP)

%.oarsh:
	export OAR_JOB_ID=`cat OAR_job_id` ; oarsh $*

%.oarsh_prog:
	export OAR_JOB_ID=`cat OAR_job_id` ; oarsh $* "tail -n 100 /tmp/viennot/err"

%.rioc_prog_one:
	ssh rioc.inria.fr "cd graph/_exec ; make $*.oarsh_prog"

besteffort:
	for param in $$(< ./paramlist); do
		oarsub -t besteffort -l core=1 "./my_script.sh $param"
	done

rioc_prog: rioc_progress

rioc_progress:
	job=`cat _rioc/last_jobid` ;\
	ssh rioc.inria.fr "cd graph/_exec ; echo '---------- err:' | cat OAR.$$job.stdout - OAR.$$job.stderr"

rioc_stat:
	job=$$(< _rioc/last_jobid) ;\
	echo $$job ;\
	ssh rioc.inria.fr "cd graph/_exec ; oarstat -fj $$job"

rioc_stop:
	job=$$(< _rioc/last_jobid) ;\
	echo $$job ;\
	ssh rioc.inria.fr "cd graph/_exec ; oardel $$job"

rioc_get:
	mkdir -p /tmp/viennot
	scp -r rioc.inria.fr:graph/_exec/_res /tmp/viennot/
	cat /tmp/viennot/_res/*.out > _res/res.out


rioc_scp:
	rsync -e ssh -v -r Makefile src oar rioc.inria.fr:graph/

rioc_scp_data:
	rsync -e ssh -v -r -L _data rioc.inria.fr:graph/



install_ocaml:
	#which ocaml || (curl -kL https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install | env OCAMLBREW_FLAGS="-r" bash)
	#which opam || (wget http://www.ocamlpro.com/pub/opam_installer.sh ; mkdir bin ; sh ./opam_installer.sh bin)
	(which ocaml && which opam && which m4) || (echo "Please install the following packages :\n  ocaml opam m4\n(may be with : sudo apt-get install ocaml opam m4 zlib1g-dev)" && (which ocaml && which opam && which m4) > /dev/null)
	opam init
	opam install camlzip || (echo "Is zlib1g-dev missing ? then try : sudo apt-get install zlib1g-dev")
	touch ocaml








