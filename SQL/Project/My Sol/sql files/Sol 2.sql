select concat(p.first_name,' ',p.last_name) as 'Full Name',p.email,p.phone
from [dbo].[Profiles] p
where marital_status = 'Y' and profile_id in (select profile_id 
                       from [dbo].[Tenancy_histories] th
					   where rent > 9000)