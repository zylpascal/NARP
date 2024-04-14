# NARP(Fortran) Manual

如果您遇到问题, feel free to open an issue on Github or you can contact us at e-mail: yangsh237@gmail.com
![Alt text](mainWindow.png)

## 运行提供的示例

1. 解压缩 narpFortran.zip
2. 运行解压缩后的文件夹中的 narpFortran.exe
3. 通过点击 **load from output** 按钮, 再选择软件压缩包中提供的**OUTPUT**示例文件, 软件会自动解析示例**OUTPUT文件**并展示
4. 或者通过点击**select folder from input files** 选择压缩包中的根目录作为输入文件夹, 并且点击**select folder from output files** 选择压缩包中的*ge根目录*作为输出文件夹,最后点击**RUN** 按钮, 软件会运行一段时间进行模拟(1~2 分钟), 自动解析输出文件并展示.

## 运行自定义文件

1. 解压缩 narpFortran.zip
2. 运行解压缩后的文件夹中的 narpFortran.exe
3. 点击**select folder from input files** 按钮选取您自定义的文件夹作为输入文件夹, 文件夹中需要且只包含LEEI, INPUTA, 和 INPUTB 这三个文件, 具体文件格式请见论文
4. 点击**select folder from output files** 按钮选择您自定义的文件夹作为输出文件夹,请保证输出文件夹为空
5. 点击**RUN** 按钮, 软件会运行一段时间进行模拟(1~2 分钟), 模拟的输出文件会被保存到您的自定义输出文件夹中, 之后软件将会自动解析输出文件(即主输出文件**OUTPUT**)并展示.
6. 如果您想从一个已存在的输出文件载入数据, 请点击 **load from output** 按钮, 再选择您的**OUTPUT**示例文件, 软件会自动解析示例**OUTPUT文件**并展示

## 文件格式定义

 请见论文

## FAQ

- 如果窗口提示: Environment Error, 可能是因为您需要重新配置您的mingw环境, 请保证您的**libgcc_s_dw2-1.dll**文件存在于您的mingw安装目录的bin文件夹下并配置好了系统环境变量.
