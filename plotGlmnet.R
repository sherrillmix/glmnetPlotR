plotGlmnet<-function(fit,markBest=FALSE,...){
	extraCex<-.85
	par(mar=c(4.1,4.8,.5,.5))
	yRange<-range(c(fit$cvup,fit$cvlo))
	devName<-tolower(fit$name)
	substring(devName,1,1)<-toupper(substring(devName,1,1))
	xRange<-rev(range(log10(fit$lambda)))
	plot(log10(fit$lambda),fit$cvm,type='n',xlim=xRange,ylim=yRange,ylab=sprintf('%s',devName),pch=21,bg='red',col=NA,las=1,xlab='',yaxt='n',xaxt='n',mgp=c(3.9,1,0),...,cex.lab=1.2)
	title(xlab=expression(paste('Model complexity (',lambda,')')),mgp=c(3.2,1,0),cex.lab=1.2)
	#text(par('usr')[1]+diff(par('usr')[1:2])*.01,fit$cvm[1],'Random',adj=c(0,.5),col='white')
	text(par('usr')[1]-diff(par('usr')[1:2])*.011,fit$cvm[1],'Random',adj=c(1,.5),xpd=NA)
	prettyY<-pretty(yRange,high.u.bias=5)
	axis(2,prettyY,rep('',length(prettyY)))
	prettyY<-prettyY[abs(prettyY-fit$cvm[1])>.003] #no overlap with random
	#axis(2,pretty(yRange),rep('',length(pretty(yRange))),las=1)
	axis(2,prettyY,las=1)
	prettyX<-pretty(log10(fit$lambda),high.u.bias=90)
	axis(1,prettyX,sapply(prettyX,function(x)as.expression(bquote(10^.(x)))),las=1)
	if(markBest)abline(v=log10(c(fit$lambda.1se)),lty=2)#fit$lambda.min,
	segments(log10(fit$lambda),fit$cvup,log10(fit$lambda),fit$cvlo,col='grey')
	stepsize<-diff(log10(fit$lambda))[1]
	segments(log10(fit$lambda)-stepsize/2,fit$cvup,log10(fit$lambda)+stepsize/2,fit$cvup,col='grey')
	segments(log10(fit$lambda)-stepsize/2,fit$cvlo,log10(fit$lambda)+stepsize/2,fit$cvlo,col='grey')
	points(log10(fit$lambda),fit$cvm,pch=21,bg='red',col='darkred',cex=.6)
	abline(h=fit$cvm[1],lty=3)
	centerPoints<-fit$cvm[1]+diff(yRange)*.04*c(-1,1)
	vertStrWidth<-strwidth('Better',cex=par('cex.main'))/diff(par('usr')[1:2])*diff(par('usr')[3:4])
	outPoints<-centerPoints+vertStrWidth*c(-1.15,1.15)#diff(yRange)*.30*c(-1,1)
	mtext('Better',2,line=2.9,at=centerPoints[1],adj=1,cex=extraCex)
	mtext('Worse',2,line=2.9,at=centerPoints[2],adj=0,cex=extraCex)
	#xCoord<-par('usr')[1]-diff(par('usr')[1:2])*.1925
	xCoord<-convertLineToUser(2.9,2)
	arrows(xCoord,centerPoints,xCoord,outPoints,xpd=NA,length=.1)
	centerPoints<-mean(par('usr')[1:2])+diff(par('usr')[1:2])*.03*c(-1,1)
	outPoints<-centerPoints+strwidth('Less complex',cex=par('cex.main'))*c(-1.15,1.1) #diff(par('usr')[1:2])*.32*c(-1,1)
	mtext('Less complex',1,line=1.95,at=centerPoints[1],adj=1,cex=extraCex)
	mtext('More complex',1,line=1.95,at=centerPoints[2],adj=0,cex=extraCex)
	#yCoord<-par('usr')[3]-diff(par('usr')[3:4])*.175
	yCoord<-convertLineToUser(2.9,1)
	arrows(centerPoints,yCoord,outPoints,yCoord,xpd=NA,length=.1)
}


plotBetas<-function(glmnet,labelProp=.01,ylab='Coefficient',transformFunc=function(x)x,...){
	par(mar=c(4,3.5,.5,.5))
	nonZeros<-apply(glmnet$beta,1,function(x)any(x!=0))
	betas<-glmnet$beta[nonZeros,]
	nVar<-apply(betas,2,function(x)sum(x>0))
	betas<-transformFunc(betas)
	#dna.R
	cols<-rainbow.lab(nrow(betas),lightScale=0,lightMultiple=.7,alpha=.8)
	plot(1,1,xlim=rev(range(log10(glmnet$lambda)))+c(0,-.2),ylim=range(betas),xaxt='n',xlab='',las=1,ylab=ylab,...,mgp=c(2.5,1,0))
	sapply(1:nrow(betas),function(x)lines(log10(glmnet$lambda),betas[x,],col=cols[x],lwd=3))
	prettyX<-pretty(log10(glmnet$lambda),high.u.bias=90)
	axis(1,prettyX,sapply(prettyX,function(x)as.expression(bquote(10^.(x)))),las=1)
	title(xlab=expression(paste('Model complexity (',lambda,')')),mgp=c(3.2,1,0),cex.lab=1.2)
	if(labelProp>0){
		selectVars<-which(betas[,min(which(nVar>nrow(betas)*labelProp))]!=transformFunc(0))
		yPos<-betas[selectVars,ncol(betas)]+diff(par('usr')[3:4])*.0075
		xPos<-par('usr')[2]-diff(par('usr')[1:2])*.005
		text(xPos,yPos,rownames(betas)[selectVars],adj=1,col=cols[selectVars])
	}
	centerPoints<-mean(par('usr')[1:2])+diff(par('usr')[1:2])*.03*c(-1,1)
	outPoints<-centerPoints+strwidth('Less complex',cex=par('cex.main'))*c(-1.15,1.1) #diff(par('usr')[1:2])*.32*c(-1,1)
	mtext('Less complex',1,line=1.95,at=centerPoints[1],adj=1,cex=.85)
	mtext('More complex',1,line=1.95,at=centerPoints[2],adj=0,cex=.85)
	#yCoord<-par('usr')[3]-diff(par('usr')[3:4])*.175
	yCoord<-convertLineToUser(2.9,1)
	arrows(centerPoints,yCoord,outPoints,yCoord,xpd=NA,length=.1)
}
