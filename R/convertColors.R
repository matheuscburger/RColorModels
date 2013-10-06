#' Convert hsv color to rgb
#' @param h hue (from 0 to 360)
#' @param s saturation (from 0 to 1)
#' @param v value (from 0 to 1)
#' @export
c.hsv2rgb <- function(h, s, v){
	Hi <- floor(h/60) %% 6
	f <- ((h/60) %% 6) - Hi
	c.p <- function(s, v) v*(1-s)
	c.q <- function(s, v, f) v*(1-f*s)
	c.t <- function(s, v, f) v*(1-(1-f)*s)
	res <- c()
	if(Hi == 0){
		res <- c(r=v, g=c.t(s, v, f), b=c.p(s, v))
	}else if(Hi == 1){
		res <- c(r=c.q(s, v, f), g=v, b=c.p(s, v))
	}else if(Hi == 2){
		res <- c(r=c.p(s, v), g=v, b=c.t(s, v, f))
	}else if(Hi == 3){
		res <- c(r=c.p(s, v), g=c.q(s, v, f), b=v) 
	}else if(Hi == 4){
		res <- c(r=c.t(s, v, f), c.p(s, v),  b=v)
	}else{
		res <- c(r=v, g=c.p(s, v), b=c.q(s, v, f))
	}
	return(res)
}



#' Convert a rgb color to hsv
#' @param r red (from 0 to 1)
#' @param g green (from 0 to 1)
#' @param b blue (from 0 to 1)
#' @export
c.rgb2hsv <- function(r, g, b){
	MAX <- max(r, g, b)
	MIN <- min(r, g, b)
	amp <- MAX - MIN
	h <- 0
	if(MAX == 0){
		return(c(h=0, s=0, v=0))
	}
	s <- (amp)/MAX
	v <- MAX
	if(amp == 0){
		return(c(h=0, s=s, v=v))
	}
	if( MAX == r ){
		if( g>= b ){
			h <- 60 * (g-b)/amp + 0
		}else{
			h <- 60 * (g-b)/amp + 360
		}
	}else if( MAX == g ){
		h <- 60 * (b-r)/amp + 120
	}else{
		h <- 60 * (r-g)/amp + 240
	}
	return(c(h=h, s=s, v=v))
}


frac2hex <- function(n){
	if(n == 0) return("00")
	scled <- n * 255
	numvec <- c()
	div <- scled
	mod <- 0
	while(div > 0){
		mod <- floor(div %% 16)
		div <- floor(div/16)
		if( mod > 9 ){
			mod <- LETTERS[mod-9]
		}
		numvec <- c(mod,numvec)
	}
	charvec <- paste0(numvec, collapse="")
	if(nchar(charvec) == 1) 
		charvec <- paste0("0", charvec)
	return(charvec)
}

#' Convert a rgb color represented by values between 0 and 1
#' in a rgb represented by Hexadecimal values
#' @param r red (from 0 to 1)
#' @param g green (from 0 to 1)
#' @param b blue (from 0 to 1)
#' @export
c.rgb2hex <- function(r, g, b){
	return(paste0("#", frac2hex(r), frac2hex(g), frac2hex(b)))	
}
