cols = 40
rows = 36

ncls.grid = 20
ncls.hour = 6

ncls.ia.h.g=-1
ncls.ia.h.l=-1
ncls.ia.h.w=-1
ncls.ia.g.l=-1
ncls.ia.g.w=-1
ncls.ia.l.w=-1

ncls.ia.h.g.l=-1
ncls.ia.h.g.w=-1
ncls.ia.h.l.w=-1
ncls.ia.g.l.w=-1

ncls.ia.h.g.l.w = -1

job.id = 2
job.group.id = 11


regression.formula = paste("cate_l1 ~", "ugrid.id.cid",

						"+ hour.cid",
#						"+ last.cate_l1",
#						"+ isweekend", 

#						"+ hour_grid.cid",
#						"+ hour_last.cid",
#						"+ hour_weekday.cid",
#						"+ grid_last.cid",
#						"+ grid_weekday.cid",
#						"+ last_weekday.cid",

#						"+ hour_grid_last.cid",
#						"+ hour_grid_weekday.cid",
#						"+ hour_last_weekday.cid",
#						"+ grid_last_weekday.cid",

#						"+ hour_grid_last_weekday.cid",

						"- 1")