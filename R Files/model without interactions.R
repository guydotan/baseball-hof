## Model without interactions ## 

set.seed(1)
d = read.csv('pitcher_list_v3.csv', stringsAsFactors=FALSE)
d$g.reg = ifelse(is.na(d$g.reg), 0, d$g.reg)
d$gs.reg = ifelse(is.na(d$gs.reg), 0, d$gs.reg)
d$cg.reg = ifelse(is.na(d$cg.reg), 0, d$cg.reg)
d$sho.reg = ifelse(is.na(d$sho.reg), 0, d$sho.reg)
d$sv.reg = ifelse(is.na(d$sv.reg), 0, d$sv.reg)
d$ipouts.reg = ifelse(is.na(d$ipouts.reg), 0, d$ipouts.reg)
d$h.reg = ifelse(is.na(d$h.reg), 0, d$h.reg)
d$er.reg = ifelse(is.na(d$er.reg), 0, d$er.reg)
d$hr.reg = ifelse(is.na(d$hr.reg), 0, d$hr.reg)
d$bb.reg = ifelse(is.na(d$bb.reg), 0, d$bb.reg)
d$so.reg = ifelse(is.na(d$so.reg), 0, d$so.reg)
d$baopp = ifelse(is.na(d$baopp), 0, d$baopp)
d$era.reg = ifelse(is.na(d$era.reg), 0, d$era.reg)
d$ibb.reg = ifelse(is.na(d$ibb.reg), 0, d$ibb.reg)
d$wp.reg = ifelse(is.na(d$wp.reg), 0, d$wp.reg)
d$hbp.reg = ifelse(is.na(d$hbp.reg), 0, d$hbp.reg)
d$bk.reg = ifelse(is.na(d$bk.reg), 0, d$bk.reg)
d$bfp.reg = ifelse(is.na(d$bfp.reg), 0, d$bfp.reg)
d$gf.reg = ifelse(is.na(d$gf.reg), 0, d$gf.reg)
d$r.reg = ifelse(is.na(d$r.reg), 0, d$r.reg)
d$sh.reg = ifelse(is.na(d$sh.reg), 0, d$sh.reg)
d$sf.reg = ifelse(is.na(d$sf.reg), 0, d$sf.reg)
d$g_idp.reg = ifelse(is.na(d$g_idp.reg), 0, d$g_idp.reg)
d$asg = ifelse(is.na(d$asg), 0, d$asg)
d$cy.young = ifelse(is.na(d$cy.young), 0, d$cy.young)
d$mvp = ifelse(is.na(d$mvp), 0, d$mvp)
d$gold.glove = ifelse(is.na(d$gold.glove), 0, d$gold.glove)
d$post.mvp = ifelse(is.na(d$post.mvp), 0, d$post.mvp)
d$w.post = ifelse(is.na(d$w.post), 0, d$w.post)
d$l.post = ifelse(is.na(d$l.post), 0, d$l.post)
d$g.post = ifelse(is.na(d$g.post), 0, d$g.post)
d$gs.post = ifelse(is.na(d$gs.post), 0, d$gs.post)
d$cg.post = ifelse(is.na(d$cg.post), 0, d$cg.post)
d$sho.post = ifelse(is.na(d$sho.post), 0, d$sho.post)
d$sv.post = ifelse(is.na(d$sv.post), 0, d$sv.post)
d$ipouts.post = ifelse(is.na(d$ipouts.post), 0, d$ipouts.post)
d$h.post = ifelse(is.na(d$h.post), 0, d$h.post)
d$er.post = ifelse(is.na(d$er.post), 0, d$er.post)
d$hr.post = ifelse(is.na(d$hr.post), 0, d$hr.post)
d$bb.post = ifelse(is.na(d$bb.post), 0, d$bb.post)
d$so.post = ifelse(is.na(d$so.post), 0, d$so.post)
d$era.post = ifelse(is.na(d$era.post), 0, d$era.post)
d$ibb.post = ifelse(is.na(d$ibb.post), 0, d$ibb.post)
d$wp.post = ifelse(is.na(d$wp.post), 0, d$wp.post)
d$hbp.post = ifelse(is.na(d$hbp.post), 0, d$hbp.post)
d$bk.post = ifelse(is.na(d$bk.post), 0, d$bk.post)
d$bfp.post = ifelse(is.na(d$bfp.post), 0, d$bfp.post)
d$gf.post = ifelse(is.na(d$gf.post), 0, d$gf.post)
d$r.post = ifelse(is.na(d$r.post), 0, d$r.post)
d$sh.post = ifelse(is.na(d$sh.post), 0, d$sh.post)
d$sf.post = ifelse(is.na(d$sf.post), 0, d$sf.post)
d$g_idp.post = ifelse(is.na(d$g_idp.post), 0, d$g_idp.post)
d$careerWAR = ifelse(is.na(d$careerWAR), 0, d$careerWAR)
d$neg.era.reg = 9 - d$era.reg
d$neg.era.post = 9 - d$era.post
## remove roger clements
d = d[d$player_id != 'clemero02', ]
## todo: remove babe ruth
d = d[d$player_id != 'ruthba01', ]
d$has.cy.young = ifelse(d$cy.young > 0, 1, 0)
d$inducted = ifelse(d$inducted == 'Y', 1, 0)
plot(d$careerWAR, d$inducted)
## note: HOF Clark Griffith is removed
d = d[!is.na(d$careerWAR) & d$careerWAR > 0, ]
d_active = d[d$max_year == 2015, ]
## remove active players
d = d[d$max_year < 2015, ]
# replicating HOF 
d_inducted = d[d$inducted == 1, ]
d = rbind(d, d_inducted, d_inducted, d_inducted, d_inducted, d_inducted, d_inducted, d_inducted, d_inducted)
mean(d$inducted)
randomized = sample(1:nrow(d))
N = nrow(d)
n = round(0.9 * N)
train_d = d[randomized[1:n],]
test_d = d[randomized[n+1:N],]
model = step(glm(inducted ~ w.reg + l.reg + sv.reg + ipouts.reg + era.reg + h.reg + er.reg + hr.reg + bb.reg + so.reg + asg + cy.young + mvp + gold.glove + post.mvp + w.post + ipouts.post + careerWAR + era_top10 + era_top5 + w_top10 + w_top5 + sv_top10 + sv_top5 + so_top10 + so_top5 + war_top10 + war_top5, family = binomial, data = train_d), trace=0, direction='both', k=log(nrow(train_d)))
summary(model)
caret::confusionMatrix(
  factor(predict(model, test_d, type = "response") > 0.5,  levels=c(F, T), labels=c('No', 'Yes')),
  factor(test_d$inducted, levels=c(0,1), labels=c('No', 'Yes'))
)
preds = cbind(d_active, inducted_pred=predict(model, d_active, type = "response"))[, c('name_first', 'name_last', 'inducted_pred')]
preds[order(preds$inducted_pred, decreasing = T), ]