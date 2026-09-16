heart_summary = data.table(
  CK_value = c(20, 60, 100, 140, 180, 220, 260, 300, 
               340, 380, 420, 460, 500),
  Heart_Attack = c(2, 13, 30, 30, 21, 19, 18, 13, 
                   19, 15, 7, 8, 35),
  No_Heart_Attack = c(88, 26, 8, 5, 0, 1, 1, 1, 
                      0, 0, 0, 0, 0)
)

heart <- rbindlist(
  lapply(seq_len(nrow(heart_summary)), function(i) {
    
    data.table(
      CK_Value = heart_summary$CK_Value[i],
      Heart_Attack = c(
        rep(1, heart_summary$Heart_Attack[i]),
        rep(0, heart_summary$No_Heart_Attack[i])
      )
    )
    
  })
)

setnames(
  heart,
  old = c("CK_Value", "Heart_Attack"),
  new = c("ck", "heart_attack")
)

heart[, id := .I]
setcolorder(heart, "id")

fwrite(heart, "lectures/data/lecture06/heart.csv")



