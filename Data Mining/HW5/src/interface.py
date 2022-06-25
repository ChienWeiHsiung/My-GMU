from PyQt5.QtWidgets import QWidget, QPushButton, QLineEdit, QListView, QListWidgetItem
from PyQt5.QtWidgets import QApplication,QVBoxLayout, QLabel, QHBoxLayout, QListWidget
from PyQt5 import QtCore
import sys
import pandas as pd
import matplotlib
matplotlib.use("Qt5Agg")
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg 
from matplotlib.figure import Figure
import matplotlib.pyplot as plt
import mplcursors


#Window of Scatter plot
class MplCanvas(FigureCanvasQTAgg):
    def __init__(self):
        fig = Figure(figsize=(10, 6), dpi=120)
        self.axes = fig.add_subplot(111)
        super(MplCanvas, self).__init__(fig)

#Scatter plot
class AnotherWindow(QWidget):

    def __init__(self, model, data):
        
        super().__init__()
        
        self.setWindowTitle(model)
        self.setFixedSize(1200, 800)
        position = self.pos()
        self.move(position.x(), position.y())
        
        plt.rcParams['font.size'] = '5'
        
        sc = MplCanvas()
        sc.axes.scatter(data['date'], data['soldprice'])
        sc.axes.set_title(model, fontsize=20)
        sc.axes.set_xlabel('Date', fontsize = 12)
        sc.axes.set_ylabel('Price', fontsize = 12)
        
        data_price = data['soldprice'].tolist()
        
        cursor = mplcursors.cursor(sc.axes, hover=True)
        cursor.connect(
            "add", lambda sel: sel.annotation.set_text(data_price[sel.target.index]))
        
        layout = QVBoxLayout()
        layout.addWidget(sc)

        self.setLayout(layout)
        

        
#Main window
class MainWindow(QWidget):

    def __init__(self, data, model_list):
        super().__init__()
        self.data = data
        self.model_list = model_list
        
        #new window
        self.new_window = ''
        
        self.setWindowTitle('homework')
        self.setFixedSize(640, 480)
        self.initUI()

    def initUI(self):
        
        self.main_layout = QVBoxLayout()
       
        self.input_layout = QHBoxLayout()
        self.label = QLabel("Input Process Type : ")
        self.input_layout.addWidget(self.label)
        self.lineEdit = QLineEdit()
        self.input_layout.addWidget(self.lineEdit)
        self.pushButton = QPushButton('Enter', self)
        self.pushButton.clicked.connect(self.button_enter)
        self.input_layout.addWidget(self.pushButton)
        self.pushButton1 = QPushButton('Clear', self)
        self.pushButton1.clicked.connect(self.button_clear)
        self.input_layout.addWidget(self.pushButton1)
        self.main_layout.addLayout(self.input_layout)
        

        self.show_layout = QHBoxLayout()
        
        self.left_layout = QVBoxLayout()
        self.label_1 = QLabel("# of Each Processor")
        self.label_1.setAlignment(QtCore.Qt.AlignCenter)
        self.left_layout.addWidget(self.label_1)
        self.listWidget_L = QListWidget()
        self.listWidget_L.setMaximumSize(QtCore.QSize(200, 600))
        self.listWidget_L.setResizeMode(QListView.Adjust)
        self.listWidget_L.itemClicked.connect(self.listwidgetclicked_left)

        self.left_layout.addWidget(self.listWidget_L)
        self.show_layout.addLayout(self.left_layout)
        
        self.right_layout = QVBoxLayout()
        self.label_2 = QLabel("Search Result")
        self.label_2.setAlignment(QtCore.Qt.AlignCenter)
        self.right_layout.addWidget(self.label_2)
        self.listWidget_R = QListWidget()
        self.listWidget_R.itemClicked.connect(self.listwidgetclicked_right)
        self.right_layout.addWidget(self.listWidget_R)
        self.show_layout.addLayout(self.right_layout)
        
        self.main_layout.addLayout(self.show_layout)
        self.setLayout(self.main_layout)
        
        self.left_list()
    
    #show the model list on the left list 
    def left_list(self):
        for i in self.model_list:
            item = QListWidgetItem( ('%4s'% ('('+ str(i['number'])+')')) + '  ' + i['type'])
            item = QListWidgetItem(item)
            self.listWidget_L.addItem(item)

    def listwidgetclicked_left(self, item):
        model = item.text().split(')')[1].strip()
        self.listwidget_reaction(model)
        
    def listwidgetclicked_right(self, item):
        model = item.text()
        self.listwidget_reaction(model)
        
    def listwidget_reaction(self, model):
        if self.new_window != '':
            self.new_window.close()
        self.new_window = AnotherWindow(model, self.data.get_group(model))
        self.new_window.show()
        
    def button_enter(self):
        self.listWidget_R.clear()
        
        possible = []
        text = self.lineEdit.text().lower()
        if text != '':
            for i in self.model_list:
                if text in i['type'].lower():
                    possible.append(i['type'])
        if len(possible) == 0:
            item = QListWidgetItem('Not found')
            self.listWidget_R.addItem(item)
        else:
            for i in possible:
                item = QListWidgetItem(i)
                self.listWidget_R.addItem(item)
        

        
    def button_clear(self):
        self.listWidget_R.clear()
        self.lineEdit.clear()


#Data of each case
data = pd.read_csv('./3.Process_data/Final_data.csv')
grouped = data.groupby('model')

#the number of cases of each processor type
model_list = pd.read_csv('./3.Process_data/Model_list.csv').to_dict('records')


app = QApplication(sys.argv)
window = MainWindow(grouped, model_list)
window.show()
sys.exit(app.exec_())