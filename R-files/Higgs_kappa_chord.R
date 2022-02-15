library(circlize)
df <- read.table(text="
 kw kz kt kb kta kg ka
kw 0.0 0.37 0.27 0.54 0.25 0.38 0.38
kz 0.0 0.0 0.17 0.50 0.37 0.03 0.56
kt 0 0 0.0 0.41 0.10 0.43 0.09
kb 0 0 0 0 0.39 0.69 0.62
kta 0 0 0 0 0 0.23 0.44
kg 0 0 0 0 0 0 0.18
ka 0 0 0 0 0 0 0 ", header = TRUE, stringsAsFactors = FALSE)
mat <- data.matrix(df)
#svg("./SM.svg") 
length(mat)
#######
state_col = c("kw" = "#DC8665",    "kz" = "#138086",
              "kt" = "#CD7672",  "kb" = "#EEB462",
              "kta" = "#534666",    "kg" = "#758EB7",
              "ka" = "#8A5082")
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

cdm_res = chordDiagram(mat,
                       directional = TRUE, annotationTrack = c("name", "grid"),
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
