from utils import config
import sys
import subprocess
from PyQt5.QtWidgets import QApplication, QMainWindow, QPushButton, QVBoxLayout, QWidget, QFileDialog, QLabel, QHBoxLayout, QTableWidgetItem, QTableWidget,QComboBox,QProgressDialog,QFrame,QMessageBox,QHeaderView
#from PyQt5.QtWidgets import *
from PyQt5.QtGui import QPixmap,QIcon,QFont
from PyQt5.QtWidgets import QLabel

from PyQt5.QtCore import QThread,pyqtSignal,Qt,QTimer
#from PyQt5.QtGui import *
from PyQt5 import sip
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg
import matplotlib.pyplot as plt
from qt_material import apply_stylesheet
import subprocess
import os
import glob
import numpy as np
inputFolder = ''
outputFolder = ''
TABLEFONTSIZE = 13
#pyqt6 样式表
class QSSLoader:
    def __init__(self):
        pass

    @staticmethod
    def read_qss_file(qss_file_name):
        with open(qss_file_name, 'r',  encoding='UTF-8') as file:
            return file.read()

class WorkerThread(QThread):
    finished = pyqtSignal()
    error = pyqtSignal(str)
    output = pyqtSignal(str)  # 新增：用于发送子进程的输出的信号
    def run(self):
        print("Thread started")  # 开始时打印

        command = ['narpexe/NA1.exe', inputFolder, outputFolder]
        print(inputFolder)  
        print(outputFolder)  
        try:
            # Set the startupinfo parameters to prevent the command-line window from showing
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            startupinfo.wShowWindow = subprocess.SW_HIDE  # 这将隐藏窗口
            result = subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,text=True,startupinfo=startupinfo)
            self.output.emit(result.stdout)  # 发送标准输出
            #print(stdout)
        except subprocess.CalledProcessError as e:
            print("chucuole1111 finished")  # 结束时打印
            self.error.emit("please check environment error, refer to the user manual")  # 发送错误信息
        except Exception as e:
            print(str(e))  # 结束时打印
            self.error.emit(str(e))  # 发送其他异常信息
        finally:
            print("Thread finished")  # 结束时打印
            self.finished.emit()  # 无论如何都发出完成信号


class MyWindow(QMainWindow):
    

    def __init__(self):
        super().__init__()
        self.outputFilePath = 'OUTPUT'
        
        self.setWindowTitle('NARP (Fortran)')
        app_icon = QIcon('imgs/icon.png')  #图标文件
        self.setWindowIcon(app_icon)
        self.setWindowOpacity(0.95) # 设置窗口透明度
        self.setAttribute(Qt.WidgetAttribute.WA_TranslucentBackground) # 设置窗口背景透明

        self.setGeometry(400, 200, 1200, 1000)

        self.setStyleSheet("border:0px solid black;border-radius:10px")
        

        #创建全局布局
        globalLayout = QVBoxLayout()
        # 创建用户输入区布局
        userInputLayout = QVBoxLayout()
        userInputLayout1 = QHBoxLayout()
        userInputLayout2 = QHBoxLayout()

        inPathButton = QPushButton(' ', self)
        inPathButton.clicked.connect(self.select_inFolder)
        self.inFolder_label = QLabel('  Please select a folder  ', self)
        self.inFolder_label.setStyleSheet("font-size: 15pt;font-weight: bold; color: black;border: none;")
        outPathButton = QPushButton('  ', self)
        outPathButton.clicked.connect(self.select_outFolder)
        self.outFolder_label = QLabel('   Please select a folder   ', self)
        self.outFolder_label.setStyleSheet("font-size: 15pt;font-weight: bold; color: black;border: none;")
        inPathButton.setStyleSheet("background-color: lightblue;background-image:url(imgs/input.png);width: 175px;height: 65px;border-radius:35px;")
        #inPathButton.setStyleSheet("border-radius:16pt");
        outPathButton.setStyleSheet("background-color: lightblue;background-image:url(imgs/output.png);width: 175px;height: 65px;border-radius:35px")
        outputFilePath = "OUTPUT"
        #run Button按下后执行
        runButton = QPushButton(' ', self)
        runButton.setStyleSheet("background-color: lightblue;background-image:url(imgs/run.png);width: 175px;height: 65px;border-radius:35px")
        runButton.clicked.connect(self.runEXE)
        #load from file 按下后直接从现存OUTPUT存取
        loadButton = QPushButton(' ', self)
        loadButton.setStyleSheet("background-color: lightblue;background-image:url(imgs/load.png);width: 175px;height: 65px;border-radius:35px;")
        loadButton.clicked.connect(self.loadOutput)
        
        userInputLayout1.addWidget(inPathButton)
        userInputLayout1.addWidget(loadButton)
        userInputLayout1.addStretch()
        userInputLayout1.addWidget(self.inFolder_label)

        userInputLayout2.addWidget(outPathButton)
        userInputLayout2.addWidget(runButton)
        userInputLayout2.addStretch()
        userInputLayout2.addWidget(self.outFolder_label)
        userInputLayout.addLayout(userInputLayout1)
        userInputLayout.addLayout(userInputLayout2)
    


        #创建输出区布局 1在左 2在右
        sysOutLayout=QHBoxLayout()
        sysOutLayout1=QVBoxLayout()
        sysOutLayout2=QVBoxLayout()
        # 创建表格布局
        table_layout1 = QVBoxLayout()
        table_layout2 = QVBoxLayout()

        style = """
        QComboBox QAbstractItemView {
        background-color: white; /* 设置背景色为白色 */
        color: black; /* 设置字体颜色为黑色 */
        
        }
        """
        #对于下拉框的提示Label
        label1 = QLabel()
        label1.setText("Statistics for Area or Pool?")
        label1.setStyleSheet("border: none;font-size:14pt;font-weight:bold;")
        # 创建下拉框
        combo_box1 = QComboBox()
        combo_box1.setFixedWidth(150)
        self.label_rep=QLabel()
        self.label_rep.setText("FINAL RESULTS AFTER ？ REPLICATION")
        self.label_rep.setStyleSheet("background-color: lightblue;font-size:14pt;font-weight:bold;")
       # self.label_rep.setFixedWidth(400)
        self.label_rep.setAlignment(Qt.AlignCenter)
        # 填充下拉框的内容从一个列表对象中
        #设置下拉框中的数据

        items1 = ["Area Summary", "Pool","Area"]
        combo_box1.clear()
        combo_box1.addItems(items1)
        combo_box1.activated.connect(self.on_combo_box1_activated)
        combo_box1.setStyleSheet(style)
        combo_box1.setStyleSheet("font-size:13pt;background-color: white;")
        # 创建用于显示选择的标签
    
        group1_layout = QHBoxLayout()
        group1_layout.addWidget(combo_box1)
        group1_layout.addWidget(self.label_rep)

        
        table_layout1.addLayout(group1_layout)




        label2=QLabel()
        label2.setText("Prob.Dist.for Selected Area?")
        label2.setStyleSheet("border: none;font-size:14pt;font-weight:bold;")
        # 创建下拉框
        self.combo_box21 = QComboBox()
        self.combo_box22 = QComboBox()
        # 填充下拉框的内容从一个列表对象中

        items22 = ["DAILY PEAK LOLES PER YEAR","HOURLY LOLES PER YEAR","ANNUAL UNSERVED ENERGY (MWH)"]
        self.combo_box21.clear()
        self.combo_box22.addItems(items22)
        self.combo_box21.activated.connect(self.on_combo_box2_activated)
        self.combo_box22.activated.connect(self.on_combo_box2_activated)
       

        self.combo_box21.setStyleSheet(style)
        self.combo_box21.setStyleSheet("font-size:13pt;background-color: white;")
        self.combo_box22.setStyleSheet(style)
        self.combo_box22.setStyleSheet("font-size:12pt;background-color: white;")
        self.combo_box22.setFixedWidth(280)
        # 创建用于显示选择的标签
        group2_layout = QHBoxLayout()
        group2_layout.addWidget(self.combo_box21)
        group2_layout.addWidget(self.combo_box22)
        group2_layout.addStretch()
        
        table_layout2.addLayout(group2_layout)

        #创建表格
        #左边表格
        self.tableWidget1 = QTableWidget(self)
        self.tableWidget1.setMinimumHeight(150)
        self.tableWidget1.setRowHeight(0,50)

        table_layout1.addWidget(self.tableWidget1)
        
        #右边表格
        self.tableWidget2 = QTableWidget(self)

        self.tableWidget2.setFixedWidth(380)

        table_layout2.addWidget(self.tableWidget2)
        

        sysOutLayout1.addWidget(label1)
        sysOutLayout1.addLayout(table_layout1)

        divider = QFrame()
        divider.setFrameShape(QFrame.Shape.VLine)
        divider.setFrameShadow(QFrame.Shadow.Sunken)
        divider.setStyleSheet("background-color: lightblue;border:none;height:310px;")
        
        sysOutLayout2.addWidget(label2)
        sysOutLayout2.addLayout(table_layout2)


        
        sysOutFrame1=QFrame()
        sysOutFrame1.setLayout(sysOutLayout1)
        sysOutLayout.addWidget(sysOutFrame1)
        sysOutLayout.addWidget(divider)
        sysOutFrame2=QFrame()
        sysOutFrame2.setFixedWidth(400)
        sysOutFrame2.setLayout(sysOutLayout2)
        sysOutLayout.addWidget(sysOutFrame2)
        
        inputFrame=QFrame()
        inputFrame.setLayout(userInputLayout)
        inputFrame.setMaximumHeight(200)
        inputFrame.setMinimumHeight(200)
        sysOutFrame=QFrame()
        sysOutFrame.setMaximumHeight(700)
        sysOutFrame.setMinimumHeight(310)
        
        sysOutFrame.setLayout(sysOutLayout)
        
        sysOutFrame.setStyleSheet("background-color: rgba(173, 216, 230, 0.1);")
        globalLayout.addWidget(inputFrame)
        globalLayout.addWidget(sysOutFrame)
        
        

         # 创建 Matplotlib 图表
        self.fig, self.ax = plt.subplots()
        self.canvas = FigureCanvasQTAgg(self.fig)
        graphFrame = QFrame()
        graphFrame.setFixedHeight(390)
        self.graphLayout = QVBoxLayout()
        image_path = 'imgs\white.png'
        # 创建 QPixmap 对象，并加载图片
        pixmap = QPixmap(image_path)
        self.imageLabel = QLabel()
        self.imageLabel.setPixmap(pixmap)
        self.imageLabel.setScaledContents(False)
        self.imageLabel.setAlignment(Qt.AlignCenter)
        scaledPixmap = pixmap.scaled(graphFrame.size(), Qt.KeepAspectRatio, Qt.SmoothTransformation)
        self.imageLabel.setPixmap(scaledPixmap)
        self.graphLayout.addWidget(self.imageLabel) 
        self.graphLayout.addWidget(self.canvas, stretch=1)
        graphFrame.setLayout(self.graphLayout)
        #self.canvas.draw()
        globalLayout.addWidget(graphFrame)


        
        container = QWidget()
        container.setStyleSheet("border: 2px solid rgba(119, 237, 229, 0.7);border-radius: 10px;")
        container.setLayout(globalLayout)
            
        self.setCentralWidget(container)
        self.setMaximumWidth(1600)
        self.setMinimumWidth(1000)
        self.envErr=False

    def select_inFolder(self):
        global inputFolder
        folder_dialog = QFileDialog.getExistingDirectory(self, 'Select Input Folder', '/path/to/default/folder')
        if folder_dialog == "":
            QMessageBox.warning(self, 'No Selection', 'You did not select a folder!')
            return
        if folder_dialog:
            self.inFolder_label.setText(f'Input Folder: {folder_dialog}')
            inputFolder = folder_dialog
    def select_outFolder(self):
        global outputFolder
        folder_dialog = QFileDialog.getExistingDirectory(self, 'Select Output Folder', '/path/to/default/folder')
        if folder_dialog == "":
            QMessageBox.warning(self, 'No Selection', 'You did not select a folder!')
            return
        if folder_dialog:
            self.outFolder_label.setText(f'Output Folder: {folder_dialog}')
            self.outputFilePath = folder_dialog+"/OUTPUT"
            outputFolder = folder_dialog
    def onFinished(self):
        self.progressDialog.close()

        # 检查子程序是否已成功完成
        if not self.envErr:
            self.updateUI()
    def onError(self, message):
        self.envErr=True
        # 显示错误信息
        QMessageBox.critical(self, 'Environment Error', 'Please check the environment of your computer and the input file format. For details, refer to the user manual.')
        self.progressDialog.close()
    def handleOutput(self, text):
        # 这里处理收到的子程序输出
        print(text)  # 例如，可以将输出打印到控制台    
        
    # DELETE FILES OF LAST SIMULATION
    def delete_specific_files(self,file_names):
        global outputFolder
        for file_name in file_names:
            # 构建搜索模式
            
            pattern = os.path.join(outputFolder,file_name)

            # 使用 glob 搜索匹配的文件
            for file in glob.glob(pattern):
                print(f"Deleting file: {file}")
                os.remove(file)  # 删除文件

    #run button clicked
    def runEXE(self):
        #运行前删除上次模拟的文件， or narp.exe 本身会自动检测到运行过了从而结束
        #file_names = ['TEMP', 'TRAOUT', 'DATOUT', 'INTMDT', 'EXTRA', 'DUMP']  # 要搜索和删除的文件名
        #self.delete_specific_files(file_names)
        self.envErr==False
        # 显示进度对话框
        self.progressDialog1 = QProgressDialog(self)
        self.progressDialog1.setWindowTitle('Please Wait')
        self.progressDialog1.setLabelText('Running simulation...')
        self.progressDialog1.setFixedWidth(400)
        self.progressDialog1.setCancelButton(None)
        self.progressDialog1.setModal(True)
        self.progressDialog1.show()
        #self.showImage()
        # 设置暂停时间为2000毫秒（2秒），然后执行self.showImage函数
        QTimer.singleShot(2000, self.showImage)
        self.progressDialog1.close()
        # 创建和启动线程
        self.thread = WorkerThread()
        self.thread.output.connect(self.handleOutput)
        self.thread.error.connect(self.onError)
        self.thread.finished.connect(self.onFinished)
        
        # 显示进度对话框
        self.progressDialog = QProgressDialog(self)
        self.progressDialog.setWindowTitle('Please Wait')
        self.progressDialog.setLabelText('Running simulation...')
        self.progressDialog.setFixedWidth(400)
        self.progressDialog.setRange(0, 0)  # 设置进度条为不确定模式
        self.progressDialog.setCancelButton(None)
        self.progressDialog.setModal(True)
        self.progressDialog.show()
        
        self.thread.start()
        print("Starting")
    def showImage(self):
        # 关闭进度对话框
        self.progressDialog1.close()
        self.imageLabel.hide()
        # 显示图片
        image_path = 'imgs/statis.png'  # 注意：根据你的操作系统，可能需要调整路径
        pixmap = QPixmap(image_path)
        
        self.imageLabel = QLabel()
        self.imageLabel.setPixmap(pixmap)
        self.imageLabel.setScaledContents(True)
        self.graphLayout.addWidget(self.imageLabel, stretch=1)  # stretch=1 会使图片随框架拉伸

    def updateUI(self):
        try:    
            # 获取必要的数据
            times = config.getTimes(self.outputFilePath)
            self.label_rep.setText(f"FINAL RESULTS AFTER {times} REPLICATIONS")

            # 设置下拉框中的数据
            area_num = config.countAreas(self.outputFilePath)
            area_list = ["Area" + str(i) for i in range(1, area_num + 1)]
            self.combo_box21.clear()
            self.combo_box21.addItems(area_list)

            # 填充第一个表格的数据
            self.table_data1 = config.getSummaryResults(self.outputFilePath)
            self.tableWidget1.setRowCount(len(self.table_data1))
            self.tableWidget1.setColumnCount(len(self.table_data1[0]))
            self.tableWidget1.verticalHeader().setVisible(False)
            self.tableWidget1.horizontalHeader().setVisible(False)
            font = QFont()
            font.setPointSize(TABLEFONTSIZE)  # 字体大小设置为12
            for i, row in enumerate(self.table_data1):
                for j, value in enumerate(row):
                    item = QTableWidgetItem(str(value))
                    item.setFont(font)
                    self.tableWidget1.setItem(i, j, item)
            self.tableWidget1.setRowHeight(0, 50)  # 设置第一行的行高
            self.tableWidget1.setColumnWidth(0, 50)  # 设置第一列的列宽

            # 填充第二个表格的数据
            self.table_data2 = config.getDailyPeakLossPerYear(self.outputFilePath, 1)
            self.tableWidget2.setRowCount(len(self.table_data2))
            self.tableWidget2.setColumnCount(len(self.table_data2[0]))
            self.tableWidget2.verticalHeader().setVisible(False)
            self.tableWidget2.horizontalHeader().setVisible(False)
            for i, row in enumerate(self.table_data2):
                for j, value in enumerate(row):
                    item = QTableWidgetItem(str(value))
                    item.setFont(font)
                    self.tableWidget2.setItem(i, j, item)
            self.tableWidget1.horizontalHeader().resizeSections(QHeaderView.ResizeToContents)
            self.tableWidget2.horizontalHeader().resizeSections(QHeaderView.ResizeToContents)
            '''
            self.tableWidget2.setColumnWidth(0,100)
            self.tableWidget2.setColumnWidth(1,130)
            self.tableWidget2.setColumnWidth(2,130)
            '''
            # 绘制图表
            self.imageLabel.hide()
            self.ax.clear()
            numbers = [int(row[0]) for row in self.table_data2[1:]]
            probabilities = [float(row[2]) for row in self.table_data2[1:]]
            self.ax.bar(numbers, probabilities)
            self.ax.set_xlabel("NUMBERS")
            self.ax.set_ylabel("PROBABILITY (%)")
            self.ax.set_title('AREA 1 DAILY PEAK LOLES PER YEAR')
            self.canvas.draw()

        except Exception as e:
            print(str(e))
            QMessageBox.warning(self, 'Error', 'An error occurred while updating the UI. Please check the format of the input files. For details, refer to the user manual.')

    def loadOutput(self):
        font = QFont()
        font.setPointSize(TABLEFONTSIZE)  # 字体大小设置为12
        options = QFileDialog.Options()
        tempPath, _ = QFileDialog.getOpenFileName(self, "选择文件", "", "All Files (*)", options=options)
        if tempPath=="":
            QMessageBox.warning(self, 'No Selection', 'You did not select a file!')
            return

        self.outputFilePath = tempPath
        
        try:
            times=config.getTimes(self.outputFilePath)
            self.label_rep.setText(f"FINAL RESULTS AFTER {times} REPLICATIONS")
            
            #   首次运行后填充组件数据
                    #设置下拉框中的数据
            area_num=config.countAreas(self.outputFilePath)
                # 使用列表推导式创建列表
            area_list = ["Area" + str(i) for i in range(1, area_num + 1)]
            items21 = area_list
            self.combo_box21.clear()
            self.combo_box21.addItems(items21)
            self.combo_box21.setStyleSheet("font-size:13pt;background-color: white;")


            #首次运行后填充表格数据
            self.table_data1=config.getSummaryResults(self.outputFilePath)
            first_row_height = 50  # 设置第一行的行高为50像素
            self.tableWidget1.setRowHeight(0, first_row_height)
            self.tableWidget1.setRowCount(len(self.table_data1))
            self.tableWidget1.setColumnCount(len(self.table_data1[0]))
            self.tableWidget1.verticalHeader().setVisible(False)
            self.tableWidget1.horizontalHeader().setVisible(False)
            
            # 填充表格数据
            for i, row in enumerate(self.table_data1):
                for j, value in enumerate(row):
                    item = QTableWidgetItem(str(value))
                    item.setFont(font)
                    self.tableWidget1.setItem(i, j, item)
            self.tableWidget1.setRowHeight(0, first_row_height)
            self.tableWidget1.setColumnWidth(0,50)
            self.tableWidget1.resizeColumnsToContents()
            # 设置表格的行数和列数
            self.table_data2=config.getDailyPeakLossPerYear(self.outputFilePath,1)
            self.tableWidget2.setRowCount(len(self.table_data2))
            self.tableWidget2.setColumnCount(len(self.table_data2[0]))
            self.tableWidget2.verticalHeader().setVisible(False)
            self.tableWidget2.horizontalHeader().setVisible(False)
            
            # 填充表格数据
            for i, row in enumerate(self.table_data2):
                for j, value in enumerate(row):
                    item = QTableWidgetItem(str(value))
                    item.setFont(font)
                    self.tableWidget2.setItem(i, j, item)

            self.tableWidget1.horizontalHeader().resizeSections(QHeaderView.ResizeToContents)
            self.tableWidget2.horizontalHeader().resizeSections(QHeaderView.ResizeToContents)
            '''
            self.tableWidget2.setColumnWidth(0,100)
            self.tableWidget2.setColumnWidth(1,130)
            self.tableWidget2.setColumnWidth(2,130)
            '''
            # 首次运行后画图
            self.imageLabel.hide()
            self.ax.clear()
            numbers = [int(row[0]) for row in self.table_data2[1:]]
            probabilities = [float(row[2]) for row in self.table_data2[1:]]
            self.ax.bar(numbers, probabilities)
            self.ax.set_xlabel("NUMBERS")
            self.ax.set_ylabel("PROBABILITY (%)")
            self.ax.set_title('AREA 1 DAILY PEAK LOLES PER YEAR')
            self.canvas.draw()
        except Exception as e:
            QMessageBox.warning(self, 'Format Error', 'Please check the format of the output file. For details, refer to the user manual.')

    def on_combo_box1_activated(self, index):
        # 获取选中的文本
        selected_text = self.sender().currentText()
        # 根据选中文本设置表中数据
        #todo
        if index == 0:
            self.table_data1=config.getSummaryResults(self.outputFilePath)
        if index == 1:
            self.table_data1=config.getPoolStatistics(self.outputFilePath)
        if index == 2:
            self.table_data1=config.getAreaResults(self.outputFilePath)
        # 更新表格数据
        remove_all_spans(self.tableWidget1)

        self.tableWidget1.setRowCount(len(self.table_data1))
        self.tableWidget1.setColumnCount(len(self.table_data1[0]))
        self.tableWidget1.verticalHeader().setVisible(False)
        self.tableWidget1.horizontalHeader().setVisible(False)
        self.tableWidget1.setRowHeight(0,50)
        font = QFont()
        font.setPointSize(TABLEFONTSIZE)  # 字体大小设置为12
        for row in range(self.tableWidget1.rowCount()):
            for col in range(self.tableWidget1.columnCount()):
                self.tableWidget1.setItem(row, col, QTableWidgetItem(""))
        for i, row in enumerate(self.table_data1):
            for j, value in enumerate(row):
                item = QTableWidgetItem(str(value))
                item.setFont(font)
                self.tableWidget1.setItem(i, j, item)
        if index>=1:
        # 合并第一列的前三个单元格
        #area no and  forcast no; 
            self.tableWidget1.setSpan(0, 0, 3, 1)
            self.tableWidget1.setSpan(0, 1,3 , 1)
        #hourly statictics
            self.tableWidget1.setSpan(0,2,1,3)
            self.tableWidget1.setSpan(1,2,2,1)
            self.tableWidget1.setSpan(1,3,2,1)
            self.tableWidget1.setSpan(1,4,2,1)
        #peak statistics
            self.tableWidget1.setSpan(0,5,1,2)
            self.tableWidget1.setSpan(1,5,2,1)
            self.tableWidget1.setSpan(1,6,2,1)
        # remarks
            self.tableWidget1.setSpan(0,7,3,1)
            
        # 设置合并后单元格的内容为前三个单元格的内容拼接
            concatenated_content1 = ' '.join([self.table_data1[i][0] for i in range(3)])
            concatenated_content1+='. '
            item = QTableWidgetItem(str(concatenated_content1))
            item.setFont(font)
            self.tableWidget1.setItem(0, 0, item)
            concatenated_content2 = ' '.join([self.table_data1[i][1] for i in range(3)])
            concatenated_content2+='. '
            item = QTableWidgetItem(str(concatenated_content2))
            item.setFont(font)
            self.tableWidget1.setItem(0, 1, item)
            concatenated_content3 = ' '.join([self.table_data1[0][i+2] for i in range(3)])
            item = QTableWidgetItem(str(concatenated_content3))
            item.setFont(font)
            self.tableWidget1.setItem(0,2, item)
            concatenated_content4 = ' '.join([self.table_data1[0][i+5] for i in range(2)])
            item = QTableWidgetItem(str(concatenated_content4))
            item.setFont(font)
            self.tableWidget1.setItem(0,5, item)
            concatenated_content5 = ' '.join([self.table_data1[1+i][2] for i in range(2)])
            item = QTableWidgetItem(str(concatenated_content5))
            item.setFont(font)
            self.tableWidget1.setItem(1,2, item)
            concatenated_content6 = ' '.join([self.table_data1[1+i][3] for i in range(2)])
            item = QTableWidgetItem(str(concatenated_content6))
            item.setFont(font)
            self.tableWidget1.setItem(1,3, item)
            concatenated_content7 = ' '.join([self.table_data1[1+i][4] for i in range(2)])
            item = QTableWidgetItem(str(concatenated_content7))
            item.setFont(font)
            self.tableWidget1.setItem(1,4, item)
            concatenated_content8 = ' '.join([self.table_data1[1+i][5] for i in range(2)])
            item = QTableWidgetItem(str(concatenated_content8))
            item.setFont(font)
            self.tableWidget1.setItem(1,5, item)
            concatenated_content9 = ' '.join([self.table_data1[1+i][6] for i in range(2)])
            item = QTableWidgetItem(str(concatenated_content9))
            item.setFont(font)
            self.tableWidget1.setItem(1,6, item)
        if index==1:
            self.tableWidget1.removeColumn(0)
        self.tableWidget1.horizontalHeader().resizeSections(QHeaderView.ResizeToContents)
        
    def on_combo_box2_activated(self):
        # 获取选中的文本
        selected_text = self.sender().currentText()
        # 获取两个ComboBox的当前选择的索引
        index1 = self.combo_box21.currentIndex()
        index2 = self.combo_box22.currentIndex()
        # 根据选中文本设置表中数据
        #todo
        if index2==0:
            self.table_data2=config.getDailyPeakLossPerYear(self.outputFilePath,index1+1)
        if index2==1:
            self.table_data2=config.getHourlyLossPerYear(self.outputFilePath,index1+1)
        if index2==2:
            self.table_data2=config.getAnnuallyUnservedEnergy(self.outputFilePath,index1+1)
        font = QFont()
        font.setPointSize(TABLEFONTSIZE)  # 字体大小设置为12
         # 更新表格数据
        for row in range(self.tableWidget2.rowCount()):
            for col in range(self.tableWidget2.columnCount()):
                self.tableWidget2.setItem(row, col, QTableWidgetItem(""))
        for i, row in enumerate(self.table_data2):
            for j, value in enumerate(row):
                item = QTableWidgetItem(str(value))
                item.setFont(font)
                self.tableWidget2.setItem(i, j, item)
        self.tableWidget2.horizontalHeader().resizeSections(QHeaderView.ResizeToContents)
        #更新统计图
        # 提取数据
        numbers = [int(row[0]) for row in self.table_data2[1:]]
        probabilities = [float(row[2]) for row in self.table_data2[1:]]
        differences = np.diff(numbers)
        min_diff = np.min(differences)
        bar_width = min_diff * 0.8
        if index2==2:
            probabilities= [float(row[2]) for row in self.table_data2[1:]]
        self.ax.clear()
        # 生成一个随机颜色
        random_color = np.random.rand(3,)  # 生成一个随机RGB颜色
        self.ax.bar(numbers, probabilities,width=bar_width,color=random_color)
        self.ax.set_xlabel("NUMBERS")
        self.ax.set_ylabel("PROBABILITY (%)")
        if index2==2:
            self.ax.set_xlabel("LIMIT (MWH)")
            self.ax.set_ylabel("PROBABILITY (%)")
        if index2==0:   
            self.ax.set_title(f'AREA {index1+1} DAILY PEAK LOLES PER YEAR')
        if index2==1:
            self.ax.set_title(f'AREA {index1+1} HOURLY LOLES PER YEAR')
        if index2==2:
            self.ax.set_title(f'AREA {index1+1} ANNUAL UNSERVED ENERGY')
        self.ax.set_xticks(numbers)  # 设置x轴刻度为数字
        self.canvas.draw()
    
    
# 使用该函数取消 tableWidget1 中的所有合并单元格
       
def remove_all_spans(table_widget):
        rows = table_widget.rowCount()
        columns = table_widget.columnCount()
        for row in range(rows):
            for column in range(columns):
                table_widget.setSpan(row, column, 1, 1)


   

def main():
    app = QApplication(sys.argv)
    window = MyWindow()
    
    
    extra = {
 
    # Button colors
    'danger': '#dc3545',
    'warning': '#ffc107',
    'success': '#17a2b8',
 
    # Font
    'font_family': '',
    }

    
    # 打开 qss 样式文件并读取类似于 CSS 的样式代码
    apply_stylesheet(app, 'light_blue.xml',invert_secondary=True,extra=extra)
    #apply_stylesheet(app, 'light_blue.xml',invert_secondary=True)
    window.show()
    sys.exit(app.exec())

if __name__ == '__main__':
    main()