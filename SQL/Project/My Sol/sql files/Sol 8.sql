select p.profile_id,concat(p.first_name,' ',p.last_name) as 'Full Name',p.phone,
case when th.rent > 10000 then 'Grade A'
     when th.rent >= 7500 and th.rent <= 10000 then 'Grade B'
	 else 'Grade C'
end [Customer Segment]
from [dbo].[Profiles] p
inner join [dbo].[Tenancy_histories] th
on p.profile_id = th.profile_id;