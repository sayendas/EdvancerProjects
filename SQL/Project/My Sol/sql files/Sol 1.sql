select top 1 p.profile_id,concat(p.first_name,' ',p.last_name) as 'Full Name',p.phone
from [dbo].[Profiles] p
inner join [dbo].[Tenancy_histories] th
on p.profile_id = th.profile_id
order by datediff(day,[move_in_date],[move_out_date]) desc
