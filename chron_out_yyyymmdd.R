   chron_out_yyyymmdd <- function( x )
     with( month.day.year( x ), sprintf( "%04.f-%02.f-%02.f", year, month, day) )
