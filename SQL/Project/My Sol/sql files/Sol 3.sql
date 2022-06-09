select p.profile_id,concat(p.first_name,' ',p.last_name) as 'FullName',p.phone,p.email,p.city,th.house_id,th.move_in_date,th.move_out_date
,th.rent,ed.latest_employer,ed.occupational_category,
count(r.referral_valid) as TotalReferrals
from [dbo].[Profiles] p
inner join [dbo].[Tenancy_histories] th
on p.profile_id = th.profile_id
inner join [dbo].[Employment_details] ed
on p.profile_id = ed.profile_id
inner join Referrals r
on p.profile_id = r.profile_id
where (p.city in ('Bangalore','Pune')) and (th.move_in_date>= '2015-01-01' and th.move_in_date<= '2016-01-01'
and th.move_out_date>= '2015-01-01' and th.move_out_date<= '2016-01-01')
group by p.profile_id,p.first_name,p.last_name,p.email,p.phone,p.city,th.house_id,th.move_in_date,th.move_out_date
,th.rent,ed.latest_employer,ed.occupational_category
order by th.rent desc