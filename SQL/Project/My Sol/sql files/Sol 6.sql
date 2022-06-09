create or alter view vw_tenant
as
select th.profile_id,th.rent,th.move_in_date,h.house_type,h.beds_vacant,a.description,a.city
,concat(a.description,', ',a.name,', ',a.city,', ',a.pincode) as 'Address'
from [dbo].[Tenancy_histories] th
inner join [dbo].[Houses] h
on th.house_id = h.house_id
inner join [dbo].[Addresses] a
on h.house_id =a.house_id
where th.move_in_date > = '2015-04-30' and h.Beds_vacant > 0


select * from vw_tenant