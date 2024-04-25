---
title: "Perceptual Data Processing"
author: "Ernestine Brannon"
output: html_document 
---








```r
# To determine the change in PSE as a function of frame size, we subtracted the PSE for counterclockwise trials from clockwise trials. A positive value indicates that participants PSEs are being biased in the direction of the tilt of the frame. 


perception<- optimized_df %>% 
  group_by(sid,frame_size,frame_orientation) %>% 
  summarize(m_pse = mean(pse)) %>% 
  mutate(frame_orientation=recode(frame_orientation, 'Right'='ccw', 'Left'='cw')) %>% 
  select(sid, frame_size, frame_orientation, m_pse) %>% 
  ungroup() %>% 
  group_by(frame_size, frame_orientation) %>% 
  pivot_wider(names_from = frame_orientation, values_from = m_pse) %>% 
  mutate(frame_effect_perception= (cw-ccw)/2)
```

```
## `summarise()` has grouped output by 'sid', 'frame_size'. You can override using
## the `.groups` argument.
```

```r
perception$frame_size<-as.factor(perception$frame_size)
```




