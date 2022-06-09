create database Tenant;

use Tenant

create table Profiles
(profile_id int identity(1,1) primary key,
 first_name varchar(255),
 last_name varchar(255),
 email varchar(255)  ,
 phone varchar(255)  ,
 city varchar(255),
 pan_card varchar(255),
 created_at date  ,
 gender varchar(255)   ,
 referral_code varchar(255),
 marital_status varchar(255))


create table Houses
(house_id int identity(1,1) primary key,
 house_type varchar(255),
 bhk_details varchar(255),
 bed_count int  ,
 furnishing_type varchar(255),
 Beds_vacant int  )


create table Tenancy_histories
(id int identity(1,1) primary key,
 profile_id int  references Profiles(profile_id),
 house_id int references Houses(house_id),
 move_in_date date  ,
 move_out_date date,
 rent int ,
 Bed_type varchar(255),
 move_out_reason varchar(255))


create table Addresses
(ad_id int identity(1,1) primary key,
 name varchar(255),
 description text,
 pincode int,
 city varchar(255),
 house_id int references Houses(house_id))


create table Referrals
(ref_id int identity(1,1) primary key,
 profile_id int  references Profiles(profile_id),
 referrer_bonus_amount float,
 referral_valid tinyint,
 valid_from date,
 valid_till date)


create table Employment_details
(id int identity(1,1) primary key,
 profile_id int  references Profiles(profile_id),
 latest_employer varchar(255),
 official_mail_id varchar(255),
 yrs_experience int,
 Occupational_category varchar(255))