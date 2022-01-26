library(circlize)
df <- read.table(text="
  h W Z A G t b c s u d τ µ e ν
h .12710 0.6483 0.8430 0.0 0.0 0.93697 0.025529 0.006936 0.000505 0.00010 0.000217 0.00923 0.00057 0.000027 0.0
W 0.0 0.0 0.5685 0.31455 0.0 0.4322 0.2161 0.4322 0.2161 0.4322 0.2161 0.6483 0.6483 0.6483 0.6483
Z 0.0 0.5685 0.0 0.0 0.0 0.12789 0.1567 0.12789 0.1567 0.12789 0.1567  0.1 0.1 0.1 0.25
A 0.0 0.0 0.0 0.0 0.0 0.2097 0.105 0.2097 0.105 0.2097 0.105 0.315 0.315 0.315 0.0
G 0.0 0.0 0.0 0.0 1.36 1.1666 1.1666 1.1666 1.1666 1.1666 1.1666 0.0 0.0 0.0 0.0
t 0.0 0.0 0. 0. 0.0 0.0 0.999146 0.0 0.0404 0.0 0.00867 0.0 0.0 0.0 0.0
b 0.0 0.0 0. 0. 0.0 0.0 0.0 0.0412 0.0 0.000351 0.0 0.0 0.0 0.0 0.0
c 0.0 0.0 0. 0. 0.0 0.0 0.0 0.0 0.97344 0.0 0.22520 0.0 0.0 0.0 0.0
s 0.00 0. 0. 0. 0. 0.0 0.0 0. 0.0 0.22534 0.0 0.0 0.0 0.0 0.0
u 0.000 0. 0. 0. .0 0.0 0.000 0.0 0.0 0.0 0.97427 0.0 0.0 0.0 0.0
d 0.000 0.0 0.0 0.0 .0 0.0 0.0 0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0
ta 0.0 0. 0. 0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
mu 0.00 0. 0. 0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
e 0.0000 0. 0. 0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
nu 0.0 0. 0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0", header = TRUE, stringsAsFactors = FALSE)
mat <- data.matrix(df)
#svg("./SM.svg") 
#######
state_col = c("h" = "#DC8665",    "W" = "#138086",
              "Z" = "#CD7672",  "A" = "#EEB462",
              "G" = "#534666",    "t" = "#758EB7",
              "b" = "#8A5082",     "c" = "#758EB7",
              "s" = "#8A5082",     "u" = "#758EB7","d" = "#8A5082",
              "τ" = "#FF7B89", "µ" = "#FF7B89",
              "e" = "#FF7B89",  "ν" = "#A5CAD2")
######
#######
state_col2 = c(state_col, state_col)
names(state_col2) = c(rownames(mat), colnames(mat))

colmat = rep(state_col2[rownames(mat)], 15)
colmat = rgb(t(col2rgb(colmat)), maxColorValue = 255)

qati = quantile(mat, 0.2)
colmat[mat > qati] = paste0(colmat[mat > qati], "A0")
colmat[mat <= qati] = paste0(colmat[mat <= qati], "20")
dim(colmat) = dim(mat)
circos.par(cell.padding = c(0, 0, 0, 0), points.overflow.warning = FALSE)

cdm_res = chordDiagram(mat, grid.col = state_col2,
                       directional = TRUE, annotationTrack = c("name", "grid"), col = colmat,
                       big.gap = 10, small.gap = 1,
                       preAllocateTracks = list(track.height = 0.1),
                       link.target.prop = FALSE)
i=1
circos.rect(cdm_res[i, "x1"], y1, cdm_res[i, "x1"] - abs(cdm_res[i, "value1"]), y1 + (y2-y1)*0.45, 
            col = col_fun(meth_mat_1[cdm_res$rn[i], cdm_res$cn[i]]), 
            border = col_fun(meth_mat_1[cdm_res$rn[i], cdm_res$cn[i]]),
            sector.index = cdm_res$rn[i], track.index = 1)
#dev.off()
# 
# a 
#circos.clear()
