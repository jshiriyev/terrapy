import numpy as np

import matplotlib.pyplot as plt

from matplotlib_venn import venn2


plt.figure(figsize=(4,4))

subsets=(30,30,15)

v2 = venn2(subsets=subsets,set_labels=('A','B'))

v2.get_patch_by_id('10').set_color('black')

plt.show()


##from matplotlib_venn import venn2, venn2_circles, venn3, venn3_circles
##
##plt.figure(figsize=(4,4))
##
##subsets=(30,10,5)
##
##v2 = venn2(subsets=subsets,set_labels=('A','B'))
##
##v2.get_patch_by_id('10').set_color('white')
##v2.get_patch_by_id('11').set_color('white')
####v3.get_patch_by_id('110').set_color('white')
####v3.get_patch_by_id('010').set_color('white')
##
##c = venn2_circles(subsets=(30,10,5),linestyle='solid')
##
##c[0].set_ls('dotted')

##subsets=(20,10,10,10,0,0,0)
##
##v3 = venn3(subsets=subsets,set_labels=('A','B','C'))
##
####v.get_patch_by_id('100').set_alpha(1.0)
##v3.get_patch_by_id('100').set_color('white')
##v3.get_patch_by_id('110').set_color('white')
##v3.get_patch_by_id('010').set_color('white')
##v3.get_patch_by_id('001').set_color('white')
##v3.get_label_by_id('100').set_text('20%')
##v3.get_label_by_id('110').set_text('10%')
##v3.get_label_by_id('010').set_text('10%')
##v3.get_label_by_id('001').set_text('10%')
##
###v.get_label_by_id('A').set_text('Set "A"')
##
##c = venn3_circles(subsets=subsets,linestyle='solid')
#c[0].set_lw(1.0)
#c[0].set_ls('dotted')

##plt.title("Sample Venn diagram")

#plt.annotate('Unknown set', xy=v.get_label_by_id('100').get_position() - np.array([0, 0.05]), xytext=(-70,-70),
#             ha='center', textcoords='offset points', bbox=dict(boxstyle='round,pad=0.5', fc='gray', alpha=0.1),
#             arrowprops=dict(arrowstyle='->', connectionstyle='arc3,rad=0.5',color='gray'))

##plt.show()
