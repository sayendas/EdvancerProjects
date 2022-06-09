update Referrals
set
valid_till= dateadd(MONTH,1,valid_till)
where
profile_id in (
select profile_id
from
Referrals
group by profile_id
having(COUNT(referral_valid) >2))


select r.profile_id,r.valid_till
from dbo.Profiles p
inner join Referrals r
on p.profile_id = r.profile_id

