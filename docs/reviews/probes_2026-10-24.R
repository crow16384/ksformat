# probes v2: verify panel findings against ksformat 0.8.5 (installed)
suppressMessages(library(ksformat))
safe <- function(lbl, expr) {
  r <- withCallingHandlers(
    tryCatch(eval(parse(text = expr)), error = function(e) paste0("ERROR: ", conditionMessage(e))),
    warning = function(w) { cat(sprintf("%-40s !warn: %s\n", lbl, conditionMessage(w))); invokeRestart("muffleWarning") })
  cat(sprintf("%-40s => %s\n", lbl, paste(format(as.vector(r)), collapse=" | ")))
}
fclear()

cat("\n--- P1/P2 overlap precedence (decl order vs sorted-low) ---\n")
safe("mk ov1", "fnew(\"3,5,TRUE,TRUE\"=\"B\", \"0,10,TRUE,TRUE\"=\"A\", type=\"numeric\", name=\"ov1\")")
safe("mk ov2", "fnew(\"0,10,TRUE,TRUE\"=\"A\", \"3,5,TRUE,TRUE\"=\"B\", type=\"numeric\", name=\"ov2\")")
safe("P1 ov1 fput(4) decl=>B?", "fput(4, 'ov1')")
safe("P2 ov2 fput(4) decl=>A?", "fput(4, 'ov2')")

cat("\n--- P3 ''-key shadowing ---\n")
safe("P3a fnew('' key)", "fnew(\"\"=\"Blank\", .missing=\"Unk\", name=\"sh1\")")
safe("P3b fparse '' rule", "fparse(text='VALUE sh2 (character)\n  \"\" = \"Blank\"\n  .missing = \"Unk\"\n;')")
safe("P3c fput('') after fparse", "fput('', 'sh2')")
safe("P3d fput(NA,'NA') F1 claim", "fput(c(NA, 'NA'), 'sh2')")

cat("\n--- P4 NA format name in vectorized fputn ---\n")
safe("mk f1", "fnew(\"0,10,TRUE,FALSE\"=\"lo\", .missing='MISSINGF1', name=\"f1\")")
safe("P4 fputn(c(5,7),c('f1',NA))", "fputn(c(5,7), c('f1', NA))")

cat("\n--- P5 POSIXct value-type range, tz drift ---\n")
safe("mk ptz canonical", "fnew(\"2024-06-01 11:00:00,2024-06-01 13:00:00,TRUE,FALSE\"=as.POSIXct('2024-06-01 12:00:00', tz='UTC'), type='POSIXct', name='ptz')")
safe("mk ptz2 brk", "fnew(\"[2024-06-01 11:00:00, 2024-06-01 13:00:00)\"=as.POSIXct('2024-06-01 12:00:00', tz='UTC'), type='POSIXct', name='ptz2')")
x_utc <- as.POSIXct("2024-06-01 12:00:00", tz="UTC")
Sys.setenv(TZ="America/New_York")
safe("P5a TZ=NY   fput(x) ptz", "format(fput(x_utc, 'ptz'), tz='UTC')")
safe("P5a2 TZ=NY  fput(x) ptz2", "format(fput(x_utc, 'ptz2'), tz='UTC')")
Sys.setenv(TZ="UTC")
safe("P5b TZ=UTC  fput(x) ptz", "format(fput(x_utc, 'ptz'), tz='UTC')")
safe("P5b2 TZ=UTC fput(x) ptz2", "format(fput(x_utc, 'ptz2'), tz='UTC')")

cat("\n--- P6 TIME11 %OS fractional seconds ---\n")
safe("P6a fputn(45000.5,'TIME11.')", "fputn(45000.5, 'TIME11.')")
safe("P6b fputn(POSIXct,'TIME11.')", "fputn(as.POSIXct('2024-01-01 12:30:15.75', tz='UTC'), 'TIME11.')")

cat("\n--- P7/P8 fparse misparse ---\n")
safe("P7 lowercase [low, high)", "names(fparse(text='VALUE a (numeric)\n  [low, high) = \"X\"\n;'))")
safe("P7b fput(4,'a')", "fput(4, 'a')")
safe("fclear a", "fclear('a')")
safe("P8 invalid date", "names(fparse(text='VALUE b (date_range)\n  [2023-13-32, 2024-01-01) = \"X\"\n;'))")
safe("P8b fput('2023-06-01' Date,'b')", "fput(as.Date('2023-06-01'), 'b')")
safe("fclear b", "fclear('b')")

cat("\n--- P9 fparse 'VALUE $SEX' ---\n")
safe("P9 parse", "fparse(text='VALUE $SEX (character)\n  \"M\" = \"Male\"\n;')")
safe("P9 registry", "paste(flist(), collapse=', ')")

cat("\n--- P10 comma-key auto type ---\n")
safe("P10a city fmt", "fnew(\"San Francisco, CA\"=\"SF\", name=\"city\")$type")
safe("P10b fput back", "fput('San Francisco, CA', 'city')")
safe("P10c thou fmt", "fnew(\"1,000\"=\"thousand\", \"2,000\"=\"twoK\", name=\"thou\")$type")
safe("P10d fput('1,000')", "fput('1,000', 'thou')")
safe("P10e fput(1500)", "fput(1500, 'thou')")

cat("\n--- P11 fputk mismatched lengths ---\n")
safe("mk ck", "fnew(\"A|1\"=\"x\", \"A|2\"=\"y\", \"B|1\"=\"z\", name=\"ck\")")
safe("P11 fputk(len2,len3)", "fputk(c('A','B'), c(1,2,NA), format='ck')")

cat("\n--- P12 huge width ---\n")
safe("P12 fnew('%10000000000.2f')", "fnew('%10000000000.2f', type='numeric', name='huge')")
safe("P12b fputn(3.14,'huge')", "fputn(3.14, 'huge')")

cat("\n--- P13 LOW/HIGH fast path sanity ---\n")
safe("P13a fparse age", "fparse(text='VALUE ag (numeric)\n  [0, 18) = \"Child\"\n  [18, 65) = \"Adult\"\n  [65, HIGH] = \"Senior\"\n  .missing = \"U\"\n;')")
safe("P13b fput edge", "fput(c(-1,0,65,200,Inf,NA), 'ag')")

cat("\n--- P14 fimport '1.0' vs '1.00' ---\n")
csv <- file.path(Sys.getenv("BH_AGENT_WORKSPACE"), "cntlo_p14.csv")
writeLines(c("FormatName,Start,End,Label,HLO,SexStart,SexEnd,Type",
             "p14,1.0,1.00,Yes,,N,N",
             "p14,2,2,No,,N,N"), csv)
safe("P14a fimport", "fimport(csv)")
safe("P14b fputn(1,'p14') expect Yes", "fputn(1, 'p14')")
safe("P14c keys", "paste(names(format_get('p14')$mappings), collapse=';')")

cat("\n--- P15 fnew_bid reverse first-wins ---\n")
safe("P15 mk", "fnew_bid(\"A\"=\"Active\", \"B\"=\"Active\", name=\"st\")")
safe("P15 finput('Active','st_inv')", "finput('Active', 'st_inv')")

cat("\n--- P16 finput nocase dup keys ---\n")
safe("P16 mk", "finput(\"Yes\"=1, \"yes\"=2, name=\"yn\", ignore_case=TRUE)")
safe("P16 finputn('YES')", "finputn('YES', 'yn')")

cat("\n--- P17 factor into numeric pattern (F12) ---\n")
safe("mk cash", "fnew(\"$%,.2f\", type=\"numeric\", name=\"cash\")")
safe("P17 fput(factor,cash)", "fput(factor(c(\"10\",\"20\")), 'cash')")

cat("\n--- P18 keep_na on pattern fmt with '' ---\n")
safe("P18 fput(c(1.5,''),cash,keep_na)", "fput(c(1.5,''), 'cash', keep_na=TRUE)")

cat("\n--- P19 NA label among expr labels (F7) ---\n")
safe("mk exl", "fnew(\"n\"=\"sprintf('%s', .x1)\", \"z\"=NA, name=\"exl\", type=\"character\")")
safe("P19 fput(c('n','z'),'exl',42)", "fput(c('n','z'), 'exl', 42)")

cat("\n--- P20 multi static+other (Q-ID-1 recheck) ---\n")
safe("mk ml2", "fnew(\"0,10,TRUE,TRUE\"=\"A\", \"0,10,TRUE,TRUE\"=\"A2\", .other=\"OTHER\", name=\"ml2\", type=\"numeric\", multilabel=TRUE)")
safe("P20 fput_all(15)", "paste(fput_all(15,'ml2')[[1]], collapse='+')")

fclear()
cat("\nPROBES DONE\n")
