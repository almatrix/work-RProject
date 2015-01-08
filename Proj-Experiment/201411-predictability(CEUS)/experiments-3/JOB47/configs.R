cols = 40
rows = 36

ncls.grid = 20
ncls.hour = 6

ncls.ia.h.g.l = 10
ncls.ia.h.g.l.w=10
ncls.ia.g.w=10
ncls.ia.h.l=10

job.id = 47
job.group.id = 41

regression.formula = paste("cate_l1 ~", "ugrid.id.cid",
						"+ hour.cid",
					
						"+ hour_grid_last.cid",
						"+ hour_grid_last_weekday.cid",
						"+ last.cate_l1",

						"+ grid_weekday.cid",
						"+ hour_last.cid",

						"- 1")