---
title: "Welcome to Tools4Xiang"
output: html_document
geometry: margin=4cm
---

#   欢迎使用"StandardCurve4qPCR"

开发这个小软件的原因很简单，要做大量的qPCR，每次都要重新做标准曲线，好麻烦啊，直接写个R Shiny好了。

# 数学推导

根据经典参考文献Analysis of relative gene expression data using real-time quantitative PCR and the 2$^{− ΔΔCT}$ method$^{[1]}$里面的公式：

$$X_n = X_0 × (1 + E_x)^n$$  (1)

其中$X_n$ 表示某个基因在$n$个扩增循环后的拷贝数（or 分子量，也就是DNA含量）；$X_0$是某个基因的起始量，也就是点样的时候加入的DNA或cDNA的量；$E_x$表示某个基因的扩增效率，理想状态是1，但是通常都不是1，不仅仅会<1，还会>1$^{[2,3]}$。

现在我们对公式(1)进行推导。

$X_n$其实就是qPCR仪器设定阈值，也就是结果多少次循环后，荧光的量达到仪器设定的检测阈值，这个值对每个基因都是一致的；$E_x$是基因引物的扩增效率，针对每个基因引物，其扩增效率是不会变的（同样的仪器、试剂、同一个人操作）；现在还剩下两个变量：$X_0$和$n$，也就是$n$的值是由$X_0$决定的，也就是最终的Cq值是由其实的DNA或cDNA的量的决定的，起始量越高，Cq（也就是$n$也就会越小）。而我们做qPCR的目的是向看不同的处理中基因的转录表达差异，简单来说就是看处理后$X_0$有没有差异。那现在的问题就变成$X_0$和$n$之间的事情了。我们把公式(1)进行简化：

$$X_n = X_0 × e^n$$

现在对两边取对数（以2为底）：

$$log_2 X_n = log_2X_0 + log_2e^n$$

再次化简：

$$log_2 X_n = log_2X_0 + n × log_2e$$

前面我们提到$X_n$和$(1 + E_x)$对每个基因的引物都是固定的，也就是$$log_2 X_n$$和$$log_2e$$是固定的，那我们再化简：

$$y = log_2x + a.n$$

此时：$$log_2X_0 = y  - a.n$$

再次强调下：在上面这个公式中，$y$和$a$对每个基因的同一个引物都是固定的，可以当作常数进行理解（处理）。

那么$log_2X_0$和$n$之间也就线性相关了。

不管$y$和$n$怎么变，$log_2X_0$的大小只和$n$相关，而且从公式中也能看出$n$越大的时候$X_0$也就越小。

问题来了，化简的时候两边取对数，底数的大小会不会影响$X_0$的值呢？拟合一个数据来探究：



这个地方要证明的是，只要有差异，log以后一定有差异，没有差异那log以后还是没有差异。



>[1] Livak K J, Schmittgen T D. Analysis of relative gene expression data using real-time quantitative PCR and the 2− ΔΔCT method[J]. methods, 2001, 25(4): 402-408.
>
>[2] Ruijter J M, Ramakers C, Hoogaars W M H, et al. Amplification efficiency: linking baseline and bias in the analysis of quantitative PCR data[J]. Nucleic acids research, 2009, 37(6): e45-e45.。
>
>[3] Rao X, Huang X, Zhou Z, et al. An improvement of the 2ˆ (–delta delta CT) method for quantitative real-time polymerase chain reaction data analysis[J]. Biostatistics, bioinformatics and biomathematics, 2013, 3(3): 71.

