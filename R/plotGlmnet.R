#' Plot model performance from cv.glmnet
#' 
#' Take a cv.glmnet object and plot out the change in performance as model complexity increases
#'
#' @param fit a cv.glmet object from \code{\link[glmnet]{cv.glmnet}} 
#' @param markBest1SE add a vertical line through the least complex model within 1 standard error of the best model
#' @param ... extra arguments to plot
#' @return NULL 
#' @export
#' @examples
#' #example from cv.glmet
#' set.seed(1010)
#' n=1000;p=100
#' nzc=trunc(p/10)
#' x=matrix(rnorm(n*p),n,p)
#' beta=rnorm(nzc)
#' fx= x[,seq(nzc)] %*% beta
#' eps=rnorm(n)*5
#' y=drop(fx+eps)
#' px=exp(fx)
#' px=px/(1+px)
#' ly=rbinom(n=length(px),prob=px,size=1)
#' set.seed(1011)
#' cvob1=glmnet::cv.glmnet(x,y)
#' plotGlmnet(cvob1)
plotGlmnet<-function(fit,markBest1SE=FALSE,...){
  extraCex<-.85
  graphics::par(mar=c(4.1,4.8,.5,.5))
  yRange<-range(c(fit$cvup,fit$cvlo))
  devName<-tolower(fit$name)
  substring(devName,1,1)<-toupper(substring(devName,1,1))
  xRange<-rev(range(log10(fit$lambda)))
  graphics::plot(log10(fit$lambda),fit$cvm,type='n',xlim=xRange,ylim=yRange,ylab=sprintf('%s',devName),pch=21,bg='red',col=NA,las=1,xlab='',yaxt='n',xaxt='n',mgp=c(3.9,1,0),...,cex.lab=1.2)
  graphics::title(xlab=expression(paste('Model complexity (',lambda,')')),mgp=c(3.2,1,0),cex.lab=1.2)
  #text(par('usr')[1]+diff(par('usr')[1:2])*.01,fit$cvm[1],'Random',adj=c(0,.5),col='white')
  graphics::text(graphics::par('usr')[1]-diff(graphics::par('usr')[1:2])*.011,fit$cvm[1],'Random',adj=c(1,.5),xpd=NA)
  prettyY<-pretty(yRange,high.u.bias=5)
  graphics::axis(2,prettyY,rep('',length(prettyY)))
  prettyY<-prettyY[abs(prettyY-fit$cvm[1])>.003] #no overlap with random
  #axis(2,pretty(yRange),rep('',length(pretty(yRange))),las=1)
  graphics::axis(2,prettyY,las=1)
  prettyX<-pretty(log10(fit$lambda),high.u.bias=90)
  graphics::axis(1,prettyX,sapply(prettyX,function(x)as.expression(bquote(10^.(x)))),las=1)
  if(markBest1SE)graphics::abline(v=log10(c(fit$lambda.1se)),lty=2)#fit$lambda.min,
  graphics::segments(log10(fit$lambda),fit$cvup,log10(fit$lambda),fit$cvlo,col='grey')
  stepsize<-diff(log10(fit$lambda))[1]
  graphics::segments(log10(fit$lambda)-stepsize/2,fit$cvup,log10(fit$lambda)+stepsize/2,fit$cvup,col='grey')
  graphics::segments(log10(fit$lambda)-stepsize/2,fit$cvlo,log10(fit$lambda)+stepsize/2,fit$cvlo,col='grey')
  graphics::points(log10(fit$lambda),fit$cvm,pch=21,bg='red',col='darkred',cex=.6)
  graphics::abline(h=fit$cvm[1],lty=3)
  centerPoints<-fit$cvm[1]+diff(yRange)*.04*c(-1,1)
  vertStrWidth<-graphics::strwidth('Better',cex=graphics::par('cex.main'))/diff(graphics::par('usr')[1:2])*diff(graphics::par('usr')[3:4])
  outPoints<-centerPoints+vertStrWidth*c(-1.15,1.15)#diff(yRange)*.30*c(-1,1)
  graphics::mtext('Better',2,line=2.9,at=centerPoints[1],adj=1,cex=extraCex)
  graphics::mtext('Worse',2,line=2.9,at=centerPoints[2],adj=0,cex=extraCex)
  #xCoord<-par('usr')[1]-diff(par('usr')[1:2])*.1925
  xCoord<-convertLineToUser(2.9,2)
  graphics::arrows(xCoord,centerPoints,xCoord,outPoints,xpd=NA,length=.1)
  centerPoints<-mean(graphics::par('usr')[1:2])+diff(graphics::par('usr')[1:2])*.03*c(-1,1)
  outPoints<-centerPoints+graphics::strwidth('Less complex',cex=graphics::par('cex.main'))*c(-1.15,1.1) #diff(par('usr')[1:2])*.32*c(-1,1)
  graphics::mtext('Less complex',1,line=1.95,at=centerPoints[1],adj=1,cex=extraCex)
  graphics::mtext('More complex',1,line=1.95,at=centerPoints[2],adj=0,cex=extraCex)
  #yCoord<-par('usr')[3]-diff(par('usr')[3:4])*.175
  yCoord<-convertLineToUser(2.9,1)
  graphics::arrows(centerPoints,yCoord,outPoints,yCoord,xpd=NA,length=.1)
  return(invisible(NULL))
}


#' Plot the coefficients from glmnet
#' 
#' Take a glmnet object and plot out the path of the coefficient for each variable as model complexity increases.
#'
#' @param glmnet a glmet object from \code{\link[glmnet]{glmnet}} 
#' @param labelLambda label all variables that are not 0 at this the closest lambda <= labelLambda
#' @param ylab label for y axis
#' @param transformFunc a functions to adjust y-axis labels (e.g. \code{exp} to show the axis as e^beta or \code{function(x)x^2} for 2^beta)
#' @param minBeta the minimum absolute necessary to count beta as different from zero
#' @param xlim minimum and maximum for limits of the x axis. Leave empty to set automatically
#' @param ylim minimum and maximum for limits of the y axis. Leave empty to set automatically
#' @param ... additional arguments for \code{\link{plot}}
#' @return NULL 
#' @export
#' @examples
#' #example from cv.glmet
#' set.seed(1010)
#' n=1000;p=100
#' nzc=trunc(p/10)
#' x=matrix(rnorm(n*p),n,p)
#' beta=rnorm(nzc)
#' fx= x[,seq(nzc)] %*% beta
#' eps=rnorm(n)*5
#' y=drop(fx+eps)
#' px=exp(fx)
#' px=px/(1+px)
#' ly=rbinom(n=length(px),prob=px,size=1)
#' set.seed(1011)
#' cvob1=glmnet::cv.glmnet(x,y)
#' plotBetas(cvob1$glmnet.fit,cvob1$lambda.1se)
plotBetas<-function(glmnet,labelLambda=0,ylab='Coefficient',transformFunc=function(x)x,minBeta=0,xlim=rev(range(log10(glmnet$lambda)))+c(0,-.2),ylim=range(betas[,inXlim]),...){
  graphics::par(mar=c(4,3.5,.5,.5))
  nonZeros<-apply(glmnet$beta,1,function(x)any(abs(x)>minBeta))
  inXlim=log10(glmnet$lambda)>=xlim[2]&log10(glmnet$lambda)<=xlim[1]
  betas<-as.matrix(glmnet$beta[nonZeros,,drop=FALSE])
  cols<-grDevices::rainbow(nrow(betas),s=.7,alpha=.8)
  graphics::plot(1,1,xlim=xlim,ylim=ylim,xaxt='n',xlab='',las=1,ylab=ylab,...,mgp=c(2.5,1,0),yaxt='n')
  sapply(1:nrow(betas),function(x)graphics::lines(log10(glmnet$lambda),betas[x,],col=cols[x],lwd=2))
  prettyX<-pretty(log10(glmnet$lambda),high.u.bias=90)
  graphics::axis(1,prettyX,sapply(prettyX,function(x)as.expression(bquote(10^.(x)))),las=1)
  prettyY<-pretty(betas[,inXlim],high.u.bias=90)
  graphics::axis(2,prettyY,sub('[.0]+$','',format(transformFunc(prettyY))),las=1)
  graphics::title(xlab=expression(paste('Model complexity (',lambda,')')),mgp=c(3.2,1,0),cex.lab=1.2)
  if(labelLambda>0){
    selectVars<-which(abs(betas[,max(which(glmnet$lambda>=labelLambda))])>minBeta)
    if(length(selectVars)>0){
      selectVars<-selectVars[order(betas[selectVars,ncol(betas)])]
      varNames<-rownames(betas)[selectVars]
      yPos<-betas[selectVars,ncol(betas)]+graphics::strheight('M')*.2
      #yPos<-transformFunc(0)+diff(par('usr')[3:4])*.0075*c(-1,1)[(betas[selectVars,ncol(betas)]<0)+1]
      #xPos<-log10(glmnet$lambda[apply(betas[selectVars,],1,function(x)max(which(x==0)))])
      offsetY<-yPos
      if(length(selectVars)>1){
        for(ii in 2:length(selectVars)){
          if(offsetY[ii]-offsetY[ii-1]<graphics::strheight(varNames[ii])){
            offsetY[ii]<-min(graphics::par('usr')[4]-graphics::strheight(varNames[ii]),offsetY[ii-1]+graphics::strheight(varNames[ii]))
          }
          offsetY[ii]<-min(graphics::par('usr')[4]-graphics::strheight(varNames[ii]),offsetY[ii]) #keep in plot
        }
      }
      xPos<-graphics::par('usr')[2]-diff(graphics::par('usr')[1:2])*.005-graphics::strwidth(varNames)*.5
      #xPos<-xPos-diff(par('usr')[1:2])*.02*rep(c(0,1),length.out=length(selectVars))[order(yPos)]
      graphics::text(xPos,offsetY,varNames,adj=c(.5,0),col=cols[selectVars])
      graphics::abline(v=log10(labelLambda),lty=2)
      graphics::segments(xPos,offsetY,log10(glmnet$lambda[ncol(betas)]),betas[selectVars,ncol(betas)],lty=2,col=cols[selectVars])
    }
  }
  centerPoints<-mean(graphics::par('usr')[1:2])+diff(graphics::par('usr')[1:2])*.03*c(-1,1)
  outPoints<-centerPoints+graphics::strwidth('Less complex',cex=graphics::par('cex.main'))*c(-1.15,1.1) #diff(par('usr')[1:2])*.32*c(-1,1)
  graphics::mtext('Less complex',1,line=1.95,at=centerPoints[1],adj=1,cex=.85)
  graphics::mtext('More complex',1,line=1.95,at=centerPoints[2],adj=0,cex=.85)
  #yCoord<-par('usr')[3]-diff(par('usr')[3:4])*.175
  yCoord<-convertLineToUser(2.9,1)
  graphics::arrows(centerPoints,yCoord,outPoints,yCoord,xpd=NA,length=.1)
  return(invisible(NULL))
}

#line: line to convert to user coordinates
#axis: axis to do conversion on (1:4 same as axis, mtext command)
convertLineToUser<-function(line,axis=1){
  if(!(axis %in% 1:4))stop(simpleError('Undefined axis'))
  axisPair<-sort((c(axis-1,axis+1)%%4)+1)
  isHeight<-(axis%%2)==1
  isSecond<-axis>2
  thisMar<-graphics::par('mar')[axis]
  marWidth<-thisMar/sum(graphics::par('mar')[axisPair])*(graphics::par('fin')-graphics::par('pin'))[isHeight+1]
  widthPerLine<-marWidth/thisMar
  #find base line + add in if plot doesn't cover whole device e.g. par(mfrow=c(2,1))
  base<-ifelse(isSecond,graphics::par('fin')[isHeight+1]-widthPerLine*thisMar,widthPerLine*thisMar) + graphics::par('fig')[1+isHeight*2]*graphics::par('din')[isHeight+1]
  func<-if(isHeight)graphics::grconvertY else graphics::grconvertX
  out<-func(base+line*widthPerLine*ifelse(isSecond,1,-1),'inches','user')
  return(out)
}
