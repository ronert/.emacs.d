;;; Compiled snippets and support files for `ess-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ess-mode
                     '(("anova" "summary(lm.sml <- lm(y ~ x1))\nsummary(lm.big <- lm(y ~ x1 + x2 + x3 + x4))\nanova(lm.sml, lm.big)" "compare fits (anova)" nil
                        ("Regression")
                        nil nil nil nil)
                       ("bar" "ggplot(timeline,aes(weekday,count,fill=weekday)) + geom_bar() + scale_fill_brewer(name='Weekday',palette=\"Set1\") + xlab('') + ylab('Number of actions')\n" "draw a barchart with ggplot2" nil
                        ("Graphics")
                        nil nil nil nil)
                       ("bygroup" "attach(fred)\nby(fred[,x:z], A, summary)" "by subgroup" nil
                        ("Tables")
                        nil nil nil nil)
                       ("cdata" "${1:designMatrix} <- model.matrix(${2:response} ~. - 1, ${3:dataFrame})\n$0" "use ascii package output from r-source" nil
                        ("caret")
                        nil nil nil nil)
                       ("cs                               " "preProcValues <- preProcess(${1:trainingData}, method = c(\"center\", \"scale\"))\ntrainDescr <- predict(preProcValues, $1)\ntestDescr <- predict(preProcValues, ${$2:testData})\n$0\n" "center and scale variables" nil
                        ("caret")
                        nil nil nil nil)
                       ("cite" "toBibtex(citation(\"${1:package}\"))" "cite package" nil
                        ("General")
                        nil nil nil nil)
                       ("ifelse" "ifelse(${1:test}, ${2:yes}, ${3:no})$0" "ifelse usage" nil
                        ("Flow Control")
                        nil nil nil nil)
                       ("countitems" "sum(v < 7, na.rm=TRUE)\n" "count number of items meeting a criterion" nil
                        ("Subsetting")
                        nil nil nil nil)
                       ("crosstab" "x<- c(1,3,1,3,1,3,1,3,4,4)     \ny <- c(2,4,1,4,2,4,1,4,2,4)     \nhmm <- table(x,y)\nprop.table(hmm,2) * 100 \n" "simple crosstabulation table" nil
                        ("Tables")
                        nil nil nil nil)
                       ("traincv" "fitControl <- trainControl(# 10-fold CV\n                            method = \"repeatedcv\",\n                            number = 10,\n                            # 5 mal 10-fold CV\n                            repeats = 5,\n                            # Alle resultate speichern\n                            returnResamp = \"all\")\n" "train using cv" nil
                        ("caret")
                        nil nil nil nil)
                       ("trainoob" "fitControl <- trainControl(# OOB um mtry zu tunen\n                           method = \"oob\",\n                           returnResamp = \"all\")" "train using cv" nil
                        ("caret")
                        nil nil nil nil)
                       ("densityplot" "ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = dbeta, args = list(n = 1, shape1 = 5, shape2 = 1))" "plot density using ggplot2" nil
                        ("ggplot2")
                        nil nil nil nil)
                       ("dfna" "${1:dataFrame}[rowSums(is.na(${2:dataFrame}))==0, ]\n$0\n" "remove rows with na´s from data frame" nil
                        ("data frames")
                        nil nil nil nil)
                       ("drawfunction" "qplot(1:10, stat = \"function\", geom = \"line\",\n      fun = function(x) dgamma(x, shape = 1))\n" "draw a function with ggplot2" nil
                        ("Graphics")
                        nil nil nil nil)
                       ("for.yasnippet" "for (${1:i} in ${2:seq}) ${3:{$0\\}}" "for.yasnippet" nil nil nil nil nil nil)
                       ("fun.yasnippet" "###\nfunction(${1:x}) ${3:{$0\\}}" "fun.yasnippet" nil nil nil nil nil nil)
                       ("search" "RSiteSearch(\"${1:searchString}\"\",\n            restrict = c(\"functions\", \"vignettes\", \"views\"),\n            format = c(\"normal\", \"short\"),\n            sortby = c(\"score\", \"date:late\", \"date:early\",\n                       \"subject\", \"subject:descending\",\n                       \"from\", \"from:descending\",\n                       \"size\", \"size:descending\"),\n            matchesPerPage = 20)\n" "search for help" nil
                        ("Help")
                        nil nil nil nil)
                       ("grabresults" "names(summary($1))$0\n" "grabbing results inside an output object" nil
                        ("Regression")
                        nil nil nil nil)
                       ("grep" "grep(\"${1:searchFor}\", ${2:string(s)})$0" "grep string(s)" nil
                        ("Text Manipulation")
                        nil nil nil nil)
                       ("help" "help(package=\"${1:package}\")" "call help for a package" nil
                        ("General")
                        nil nil nil nil)
                       ("ife.yasnippet" "ifelse(${1:test}, ${2:yes}, ${3:no})" "ife.yasnippet" nil nil nil nil nil nil)
                       ("covmat" "makecov <- function(rho,n) {\n   m <- matrix(nrow=n,ncol=n)\n   m <- ifelse(row(m) == col(m),1,rho)\n   return(m)\n}" "generate covariance matrix" nil
                        ("Matrices")
                        nil nil nil nil)
                       ("labeledscatter" "exp <- ggplot(exp05, aes(x=number, y=length, label=country, size=receipt, colour = region))\nexp + geom_point() + geom_text(hjust=0.7, vjust=2) + labs(x = \"Number of Tourist Arrivals\", y = \"Length of Stay (days)\") + scale_area(\"Receipt (M. USD)\") + scale_colour_hue(\"Region\")\n" "draw a labeled scatterplot with ggplot2" nil
                        ("Graphics")
                        nil nil nil nil)
                       ("lib.yasnippet" "library(\"${1:libname}\")\n$0" "lib.yasnippet" nil nil nil nil nil nil)
                       ("lm" "lm($1 ~ $2, data = $3)$0\n" "linear model" nil
                        ("Regression")
                        nil nil nil nil)
                       ("lmfactor" "R <- lm(y ~ grp:x, data=foo)" "calculate seperate coefficients for each level of a factor" nil
                        ("Regression")
                        nil nil nil nil)
                       ("logit" "$1 <- glm($2 ~ $3, family=binomial(link=logit))$0" "fit a logit model       " nil
                        ("Regression")
                        nil nil nil nil)
                       ("matcombine" "do.call(\"rbind\", ${1:listOfMatrices})$0\n" "combine a lot of matrices efficiently" nil
                        ("Matrices")
                        nil nil nil nil)
                       ("search" "RSiteSearch(\"${1:searchString}\"\",\n            restrict = c(\"functions\", \"vignettes\", \"views\"),\n            format = c(\"normal\", \"short\"),\n            sortby = c(\"score\", \"date:late\", \"date:early\",\n                       \"subject\", \"subject:descending\",\n                       \"from\", \"from:descending\",\n                       \"size\", \"size:descending\"),\n            matchesPerPage = 20)\n" "search for help" nil
                        ("Help")
                        nil nil nil nil)
                       ("sub" "X[Y > A & Y <= B]\n" "multiple subsetting conditions" nil
                        ("Subsetting")
                        nil nil nil nil)
                       ("mvn" "mvrnorm(2, c(0,0), matrix(c(0.25, 0.20, 0.20, 0.25), 2,2))" "simulate multivariate normal" nil
                        ("Simulation")
                        nil nil nil nil)
                       ("nao.yasnippet" "na.omit(${0:})" "nao.yasnippet" nil nil nil nil nil nil)
                       ("read.yasnippet" "read.table(\"${1:filename}\"${2:, header = ${3:TRUE},  sep = \"${4:\\t}\",  stringsAsFactors = ${5:FALSE}})" "read.yasnippet" nil nil nil nil nil nil)
                       ("seq.yasnippet" "seq(${1:from}, ${2:to}, ${3:by})" "seq.yasnippet" nil nil nil nil nil nil)
                       ("smooth" "ggplot(res,aes(hour,value)) + geom_smooth(aes(group=variable,colour=factor(variable)),alpha=0.3,size=2) + xlab('Hour') + ylab('Median') + scale_color_brewer(name='Day',palette=\"Set1\")\n" "draw a smoother with ggplot2" nil
                        ("Graphics")
                        nil nil nil nil)
                       ("sortdf" "${1:dataframe} <- $1[order($1$${2:var1}, ${3:var2}), ]" "sort data frame by more than one variable" nil
                        ("Data Manipulation")
                        nil nil nil nil)
                       ("sou.yasnippet" "source(${1:\"${2:}\"}${3:, chdir = ${4:TRUE}})" "sou.yasnippet" nil nil nil nil nil nil)
                       ("stop" "stop(\"${1:errorString}\")$0" "stop usage" nil
                        ("Error Handling")
                        nil nil nil nil)
                       ("stopifnot" "stopifnot(is.logical(na.rm))" "stopifnot usage" nil
                        ("Error Handling")
                        nil nil nil nil)
                       ("tikz" "tikz(file = \"${1:path}\", width = 5, height\n       = 5, engine = \"xetex\")\n  ${2:plotName}\n  dev.off()\n  $0\n" "create tikz plot" nil
                        ("Graphics")
                        nil nil nil nil)
                       ("todo" "### TODO: " "create todo in source" nil
                        ("General")
                        nil nil nil nil)
                       ("triangular" "# You want:\n# a^0  0    0\n# a^1  a^0  0\n# a^2  a^1  a^0\n\n# At least 4 ways:\nma <- matrix(rep(c(a^(0:n), 0), n+1), nrow=n+1, ncol=n+1)\nma[upper.tri(ma)] <- 0\n\nn <- 3\na <- 2\nout <- diag(n+1)\nout[lower.tri(out)] <-\na^apply(matrix(1:n,ncol=1),1,function(x)c(rep(0,x),1:(n-x+1)))[lower.tri(out)]\nout\n\nfun <- function(x,y) { ifelse(y>x, x+y, 0) } \nfun(2,3) \nouter(1:4, 1:4, fun) \n\na <- 0.2\nfun <- function(x, y) { ifelse(y<=x, a^(x+y-1), 0) }\nouter(1:4, 1:4, fun)\n" "create Upper or Lower Triangular Matrix" nil
                        ("Matrices")
                        nil nil nil nil)
                       ("tryCatch" "tryCatch()" "tryCatch usage" nil
                        ("Error Handling")
                        nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:13 2013
