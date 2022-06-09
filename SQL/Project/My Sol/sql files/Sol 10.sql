select h.[house_id],h.[house_type],h.[bhk_details],h.[bed_count],h.Beds_vacant,h.[furnishing_type],(h.bed_count-h.Beds_vacant) Occupancy
from [dbo].[Houses] h
where h.bed_count-h.Beds_vacant in (select max(h.bed_count-h.Beds_vacant)
                                  from dbo.Houses h)

