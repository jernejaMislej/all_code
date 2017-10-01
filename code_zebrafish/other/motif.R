bases = c("A","C","G","T")
motif = "AGGATCTAACCG"
genomeLen = 1E6

# replicate often enough to get a reasonable precision
n = replicate(1000, {
	# sample a new random genome sequence of the specified size
	genomeSeq = sample(bases, size = genomeLen, replace = TRUE)
	# convert the vector of characters into a character string
	genomeSeq = paste0(genomeSeq, collapse="")
	# find all the occurences of motif within genomeSeq
	matches = gregexpr(motif,genomeSeq)[[1]]
	# return the number of these occurences
	if (matches[1]==-1) 0 else length(matches)
})

# these are the estimated probabilities of 0, 1, 2, ... k occurrences:
table(n)/length(n)

