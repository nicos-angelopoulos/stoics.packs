
:- <- library( "RColorBrewer"  ).

columns_fisher_test_defaults( plot(false) ).

% this is kept in for backward compatibility. please remove early in 2016
cross_column_fisher_test( Data, Inters, Odds, Lods ) :-
	cross_column_fisher_test( Data, Inters, Odds, Lods, [] ).

/** columns_fisher_test( +Data, +Inters, +Odds, -Lods, +Opts ).

Perform pairwise Fisher tests on the columns of Data (mtx/1). 

Opts

  * plot(Plot=false)
    whether to plot the test results

  * plot_on(PlotOns)
    see pl_plot_on/2, multiple allowed- but Plot needs to be true for them to fire

==
?- cd( '/usr/local/users/nicos/work/2015/15.11.04-hmrn/Exploristics/data' ).
?- columns_fisher_test( hmrn_muts_t10.csv, inters, odds, Lods, [plot(true),plot_on(x11()),plot_on(pdf())] ).
==

@author nicos angelopoulos
@version  0.1 2015/12/16

*/
columns_fisher_test( DataIn, Inters, Odds, Lods, Args ) :-
	r_data_frame( DataIn, Data ),
	options_append( columns_fisher_test, Args, Opts ),
	Cols   <- ncol(Data), 
	Inters <- matrix( ncol=Cols, nrow=Cols ),
	Odds   <- matrix( ncol=Cols, nrow=Cols ),
	% this is quite wasteful, the test seems symmetric j -> 1,i instead to of 1,N
	cross_column_fisher_test( 1, 2, Cols, Data, Inters, Odds ),
	diag( Inters ) <- 0,
	diag( Odds )   <- 1,
	rownames( Inters ) <- colnames( Data ),
	colnames( Inters ) <- colnames( Data ),
	dot( Inters, bh, Intersbh ),
	% Intersbh <- interactions,
	Intersbh <- Inters,
     Intersbh[lower.tri(Intersbh)] <- p.adjust(10^ - (abs(Intersbh[lower.tri(Intersbh)])), method="BH"),
  	Intersbh[upper.tri(Intersbh)] <- p.adjust(10^ - abs(Intersbh[upper.tri(Intersbh)]), method="BH"),
  	diag(Intersbh) <- 1,
	Odds[10^ -abs(Inters) > 0.05 ; Intersbh > 0.1] <- 1,
	MinOdds <- min( Odds[Odds \= 0], na.rm='T' ),
	Odds[Odds == 0] <- MinOdds,
	% Odds[Odds == 0] <- min(Odds[Odds \= 0]),
	% trace,
	% fixme: we assume all NAs are from +Inf ... check and check again
	Odds[is.na(Odds)] <- MinOdds,
	% atomic_list_concat( [log,Odds], '_', Lods ),
	dot( log, Odds, Lods ),
	Lods <- log(Odds),
	options( plot(Plot), Opts ), 
	columns_fisher_test_plot_on_option( Plot, Data, Inters, Odds, Lods, Opts ).

cross_column_fisher_test( I, J, Cols, Data, Inters, Odds ) :-
	I > Cols,
	NxJ is J + 1,
	!,
	cross_column_fisher_test_nest( NxJ, Cols, Data, Inters, Odds ).
cross_column_fisher_test( I, J, Cols, Data, Inters, Odds ) :-
	% debug( hmrn, 'I: ~d, J: ~d', cross_column_fisher_test[I,J] ),
	% ( ((I =:= 65;I=:=64), J=:=1) -> trace; true ),
	/*
	  interactions <- sapply(1:ncol(genomic_data), function(i) sapply(1:ncol(genomic_data), function(j) {f<- try(fisher.test(genomic_data[,i], genomic_data[,j]), silent=TRUE); if(class(f)=="try-error") 0 else ifelse(f$estimate>1, -log10(f$p.val),log10(f$p.val))} ))
	*/
	f <- try(fisher.test(Data[*,I], Data[*,J]), silent='TRUE'),
	% f <- try(fisher.test(Data[*,I], Data[*,J],workspace=800000,simulate.p.value='TRUE'), silent='TRUE'),
	% write( try(fisher.test(Data[*,I], Data[*,J],workspace=800000,simulate.p.value='TRUE'), silent='TRUE') ),
	% write( I:J ), nl,
	% write( f <- try(fisher.test(Data[*,I], Data[*,J],workspace=800000), silent='TRUE') ),

	fisher_log10_odds( f, I, J, Log10Val, OddVal ),
	Inters[I,J] <- Log10Val,
	IsInf <- is.infinite(OddVal),
	( IsInf == true ->
		Odds[I,J]   <- 'NA'
		;
		Odds[I,J]   <- OddVal
	),
	NxI is I + 1,
	cross_column_fisher_test( NxI, J, Cols, Data, Inters, Odds ).

columns_fisher_test_plot_on_option( true, Data, Inters, Odds, Lods, Opts ) :-
	Goal = cross_column_fisher_test_plot(Data,Inters,Odds,Lods),
	maplist( pl_plot_on(Goal), Opts ).
columns_fisher_test_plot_on_option( false, _Data, _Inters, _Odds, _Lods, _Opts ).

% this is kept in for backward compatibility. please remove early in 2016
cross_column_fisher_test_plot( Data, Inters, Odds, Lods, PdfF ) :-
	<- pdf( +PdfF ),
	cross_column_fisher_test_plot( Data, Inters, Odds, Lods ),
	<- dev.off().

cross_column_fisher_test_plot( _Data, Inters, _Odds, Lods ) :-
	old.par <- par(),
	<- par(bty="n", mgp = c(2,0.5,0), mar=c(3,3,2,2)+0.1, las=2, tcl= -0.33),
     ix <- colnames(Inters),
	h <- hclust(dist(Inters[ix,ix])),
	o <- h$order,
    <- image(x=1:ncol(Lods), y=1:nrow(Lods), Lods[o,o], col=brewer.pal(11,"BrBG"), breaks = c(-200, seq(-3,3,2/3),  200), xaxt="n", yaxt="n", xlab="",ylab="", xlim=c(0, ncol(Lods)+3), ylim=c(0, ncol(Lods)+3)),
  % #image(x=1:ncol(interactions), y=1:nrow(interactions), log10(odds[o,o]), scale="none", col=brewer.pal(11,"BrBG"), breaks = seq(-3,3,length.out=12), xaxt="n", yaxt="n", xlab="",ylab="", xlim=c(0, ncol(interactions)+1))
	<- mtext(side=1, at=1:ncol(Inters), colnames(Inters)[o], cex=0.5, font=c(rep(3,ncol(Inters)))),
	<- mtext(side=2, at=1:ncol(Inters), colnames(Inters)[o], cex=0.5, font=c(rep(3,ncol(Inters)))),
     <- abline(h = length(h$order)+0.5, col="white"),
     <- abline(v = length(h$order)+0.5, col="white"),
	dot( Inters, bh, Intersbh ),
	w <- arrayInd(which(Intersbh[o,o] <= 0.1 & Intersbh[o,o] > 0.05), rep(nrow(Inters),2)),
	<- points(w, pch=20, col="white", cex=0.5),
	w <- arrayInd(which(Intersbh[o,o] <= 0.05 & Intersbh[o,o] > 0.01), rep(nrow(Inters),2)),
  	<- points(w, pch=3, col="white", cex=0.25),
  	w <- arrayInd(which(Intersbh[o,o] <= 0.01), rep(nrow(Inters),2)),
  	<- points(w, pch=5, col="white", cex=0.25),
  	<- image(y = 1:11, x=rep(ncol(Inters),2)+c(2,3), z=matrix(c(1:11), nrow=1), col=brewer.pal(11,"BrBG"), add='TRUE'),
	<- axis( side=4, at = c(1,6,11), cex.axis=0.5, tcl= -0.15, label=c(signif(exp(min(Lods)), digits=1), "OR=1", signif(exp(max(Lods)), digits=2)), las=1, lwd=0.5),
	<- lines(rep(ncol(Inters),2)+c(1,4), c(6,6)+0.5, col="white"),
  	% <- mtext(side=5, at=0,  "Mutually\nexclusive", cex=0.5, line= -1),
  	<- mtext(side=4, at= -1,  "Mutually\nexclusive", cex=0.5, line= -1, adj=0),
	<- mtext(side=4, at=12,  "Co-mutated", cex=0.5, line= -1).

fisher_log10_odds( Fvar, I, J, Log10Val, OddVal ) :-
	Error <- class(Fvar),
	% ( Error == 'try-error' ; (Est <- Fvar$estimate,  Est == []) ), % double check the RHS of ; (own addition)
	Error == 'try-error',
	!,
	write( f_error(I,J) ), nl,
	Log10Val is 0,
	OddVal = 'NA'.  % check
fisher_log10_odds( Fvar, _I, _J, Log10Val, OddVal ) :-
	% <- print( f$estimate ),
	Est <- Fvar$estimate,
	Est \== [],
	Est > 1,
	!,
	% <- print(Fvar$estimate),
	Pval <- Fvar$p.val,
	% % PvalPrv <- Fvar$p.val,
	% debug( _, 'Pval: ~w', PvalPrv ),
	% % ( PvalPrv =:= 0 -> Pval is 10e-10; Pval = PvalPrv ),
	% % % Log10Val is - log10(Pval),
	Log10Val <- - log10(Pval),
	OddVal = Fvar$estimate.
	% OddVal <- Fvar$estimate.
fisher_log10_odds( Fvar, _I, _J, Log10Val, OddVal ) :-
	Pval <- Fvar$p.val,
	Log10Val is log10(Pval),
	% OddVal <- Fvar$estimate.
	% <- print(Fvar$estimate),
	Est <- Fvar$estimate,
	( Est == [] ->
		OddVal = 'NA'
		;
		OddVal = Fvar$estimate
	).

cross_column_fisher_test_nest( J, Cols, _Data, _Inters, _Odds ) :-
	J > Cols,
	!.
cross_column_fisher_test_nest( J, Cols, Data, Inters, Odds ) :-
	cross_column_fisher_test( 1, J, Cols, Data, Inters, Odds ).
