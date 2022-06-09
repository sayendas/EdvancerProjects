select city,rent,sum(rent) over (partition by city order by id) as 'rent total by city'
,sum(rent) over (order by id) as 'running sum of rent for all cities'
from [dbo].[Profiles] p
inner join [dbo].[Tenancy_histories] th
on p.profile_id = th.profile_id

