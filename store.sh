
cd /data/sdm_emv_finance/outputs && rm sdm.zip && zip sdm.zip *.tif

scp rouf1703@pose.vhost33:/data/sdm_emv_finance/outputs/sdm.zip /home/frousseu/Documents/github/sdm_emv_finance/outputs/

scp rouf1703@pose.vhost33:/data/sdm_emv_finance/outputs /home/frousseu/Documents/github/sdm_emv_finance/outputs

s5cmd cp -acl public-read '/home/frousseu/Documents/github/sdm_emv_finance/outputs/*.tif' s3://bq-io/acer/sdm_emv_finance/

# https://object-arbutus.cloud.computecanada.ca/bq-io/acer/sdm_emv_finance