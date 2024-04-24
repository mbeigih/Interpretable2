hist(cervical$Age)
min(cervical$Age)
max(cervical$Age)
mean(cervical$Age)

str(cervical)

table(cervical$Number.of.sexual.partners)
hist(cervical$Number.of.sexual.partners)

table(cervical$First.sexual.intercourse)
hist(cervical$First.sexual.intercourse)
mean(cervical$First.sexual.intercourse)

table((cervical$Num.of.pregnancies))
hist(cervical$Num.of.pregnancies)

table(cervical$Smokes)

table(cervical$Smokes..years.)
hist(cervical$Smokes..years.)
hist(cervical$Smokes..years.[cervical$Smokes..years.!=0])
max(cervical$Smokes..years.)

table(cervical$Hormonal.Contraceptives)
mean(cervical$Hormonal.Contraceptives..years.[cervical$Hormonal.Contraceptives..years.!=0])
hist(cervical$Hormonal.Contraceptives..years.[cervical$Hormonal.Contraceptives..years.!=0])
max(cervical$Hormonal.Contraceptives..years.)

table(cervical$IUD)
hist(cervical$IUD..years.[cervical$IUD..years.!=0])
max(cervical$IUD..years.)

table(cervical$STDs)

table(cervical$STDs..number.)

table(cervical$STDs..Time.since.first.diagnosis)

table(cervical$STDs..Time.since.last.diagnosis)

table(cervical$Biopsy,cervical$Smokes,cervical$Hormonal.Contraceptives)
