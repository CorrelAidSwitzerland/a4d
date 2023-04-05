codes_in_2nd_row <- data.frame(column1=c('test','test1','test2'),
                               column2=c('test','country_DE','clinic_GR_dd'),
                               column3=c('test','test1','test2'))

codes_in_column_names <- data.frame(column1=c('test','test1','test3'),
                                    country_DE=c('test','test2','test2'),
                                    clinic_GR=c('test','test1','test2'))

codes_missingCountry <- data.frame(column1=c('test','test1','test3'),
                                   column2=c('test','test2','test2'),
                                   clinic_GR=c('test','test1','test2'))

codes_notclean <- data.frame(column1=c('test','test1','test2'),
                             column2=c('test','blabla.country_DE.blabla.','blabla_clinic_GR_bla'),
                             column3=c('test','test1','test2'))
