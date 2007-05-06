
.SUFFIXES: .zomp .ll .check

.zomp.check:
	@zomp-check-syntax.sh $< || cat ${<:.zomp=.ll}

