select concat(p.first_name,' ',p.last_name) as 'Full Name',p.phone,p.city
,h.house_id,h.house_type,h.bhk_details,h.bed_count,h.furnishing_type,h.Beds_vacant
from [dbo].[Profiles] p
inner join [dbo].[Tenancy_histories] th
on p.profile_id = th.profile_id
inner join [dbo].[Houses] h
on h.house_id = th.house_id
where p.profile_id not in (select profile_id 
                         from [dbo].[Referrals]);