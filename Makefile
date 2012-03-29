all: compile

compile:
	sml -m bq.cm Compile.sml

test:
	echo "val _ = OS.Process.exit OS.Process.success" | sml -m bq.cm SmlUnit.sml DcTest.sml
