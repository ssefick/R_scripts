community_round2int <- function(L){

row <- rownames(L)

a <- as.data.frame(apply(L, 2, round))

b <- as.data.frame(apply(a, 2, as.integer))

rownames(b) <- row

return(b)

}
