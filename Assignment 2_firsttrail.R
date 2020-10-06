# Load the package
library(RODBC)

# Connect to MySQL (my credentials are mysql_server_64/root/.)
db = odbcConnect("mysql_server_64", uid="root", pwd="9844")
sqlQuery(db, "USE ma_charity_full")

# Extract calibration data from database
query = "SELECT c.contact_id,c.donation,c.amount AS 'donation_amount', recency,frequency,avgamount,maxamount
         FROM assignment2 c
         LEFT JOIN (SELECT contact_id,DATEDIFF(20180626, MAX(act_date)) / 365 AS 'recency',
                    COUNT(amount) AS 'frequency',
                    AVG(amount) AS 'avgamount',
                    MAX(amount) AS 'maxamount'
                    FROM acts
                    WHERE (act_type_id = 'DO')
                    GROUP BY contact_id) AS a
         ON c.contact_id = a.contact_id
         WHERE (c.calibration = 1)"

data = sqlQuery(db, query)

# Show data
print(head(data))

# In-sample, probability model
library(nnet)
prob.model = multinom(formula = donation ~ (recency * frequency) + log(recency) + log(frequency),
                      data = data)

# In-sample, donation amount model
# Note that the amount model only applies to a subset of donors...
z = which(!is.na(data$donation_amount))
print(head(data[z, ]))
amount.model = lm(formula = log(donation_amount) ~ log(avgamount) + log(maxamount),
                  data = data[z, ])

# Extract prediction data from database
query2 = "SELECT c.contact_id, recency,frequency,avgamount,maxamount
         FROM assignment2 c
         LEFT JOIN (SELECT contact_id,DATEDIFF(20180626, MAX(act_date)) / 365 AS 'recency',
                    COUNT(amount) AS 'frequency',
                    AVG(amount) AS 'avgamount',
                    MAX(amount) AS 'maxamount'
                    FROM acts
                    WHERE (act_type_id = 'DO')
                    GROUP BY contact_id) AS a
         ON c.contact_id = a.contact_id
         WHERE (c.calibration = 0)"

pred_data = sqlQuery(db, query2)
print(head(pred_data))

# Close the connection
odbcClose(db)

# Out-of-sample predictions
# Do NOT forget to re-transform "log(amount)" into "amount"
out = data.frame(contact_id = pred_data$contact_id)
out$probs  = predict(object = prob.model, newdata = pred_data, type = "probs")
out$amount = exp(predict(object = amount.model, newdata = pred_data))
out$score  = out$probs * out$amount

# Show results
print(head(out))

# Who is likely to be worth more than 5 EUR?
out$solicit = 0
out$solicit = ifelse(out$score > 2,1,0)

#Results
final_results <- data.frame(out$contact_id,out$solicit)
colnames(final_results) <- NULL

#text file
write.table(final_results, "arpit", sep = "\t", dec = ".")