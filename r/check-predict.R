# check predictions against previous version

df1 <- df %>%
  select(
    featureid,
    current = occ_current,
    plus2 = occ_temp7p20,
    plus4 = occ_temp7p40,
    plus6 = occ_temp7p60,
    max_temp_0.3 = max_temp7p_occ30,
    max_temp_0.5 = max_temp7p_occ50,
    max_temp_0.7 = max_temp7p_occ70
  ) %>%
  gather(var, value, -featureid) %>%
  mutate(version = "new")

df2 <- read.table("~/Projects/sheds/data/bto-model/20160321/Occupancy_Predictions.csv", header = TRUE, sep = ",") %>%
  select(featureid, current, starts_with("plus"), starts_with("max_temp")) %>%
  gather(var, value, -featureid) %>%
  mutate(version = "old")

df3 <- bind_rows(df1, df2) %>%
  spread(version, value)

df3 %>%
  filter(var == "current") %>%
  ggplot(aes(old, new)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE)

df3 %>%
  filter(var == "plus6") %>%
  ggplot(aes(old, new)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_abline(color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE)

df3 %>%
  filter(var == "max_temp_0.5") %>%
  ggplot(aes(old, new)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_abline(color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE)
