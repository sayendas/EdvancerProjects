select concat(p.first_name,' ',p.last_name) as 'Full Name',p.email,p.phone,p.referral_code
,case when r.referral_valid =1 then sum(r.referrer_bonus_amount)
      when r.referral_valid = 0 then 0 
end as 'TotalBonusReferralAmount'
from [dbo].[Profiles] p
inner join [dbo].[Referrals] r
on p.profile_id = r.profile_id
group by concat(p.first_name,' ',p.last_name),p.email,p.phone,p.referral_code,r.referral_valid
having count(p.referral_code) > 1