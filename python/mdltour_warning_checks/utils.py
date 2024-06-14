from loguru import logger
from datetime import datetime
import pandas as pd
import adlfs
import os
from typing import List
from dataclasses import dataclass

logging_dir = r'I:\NTS\outputs\tour\checks_log'

@dataclass
class ChecksLogger:
    log_file_name: str
    logs: List[List[str]] = None

    def __post_init__(self):
        self.run_id = int(datetime.utcnow().timestamp())
        self.start_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        self.headers = ['run_id', 'timestamp', 'level', 'test_id', 'message']
        self.init_path()

        if self.logs is None:
            self.logs = []
    
    def init_path(self):
        if self.log_file_name.split('.')[-1] != 'csv':
            self.log_file_name = self.log_file_name.split('.')[0] + '.csv'

        self.path = logging_dir + '{self.log_file_name}'

        if not self.__log_file_exists__():
            self.initialize_log()
    
    def initialize_log(self):
        """Initializes the log file with headers."""
        pd.DataFrame(columns=self.headers).to_csv(self.path, index=False)
    
    def log(self, message: str, test_id: str, level: str = 'INFO'):
        """
        Adds a log message to the current logs list.
        
        Args:
            message (str): The log message to add.
            level (str, optional): The log level. Defaults to 'INFO'.

        """
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        """Adds a log message to the current logs list."""
        self.logs.append([self.run_id, timestamp, level, test_id, message])

    def success(self, test_id: str, message: str):
        """Adds a success message to the current logs list."""
        self.log(message, 'SUCCESS')

    def info(self, test_id: str, message: str):
        """Adds an info message to the current logs list."""
        self.log(message, 'INFO')

    def warning(self, test_id: str, message: str):
        """Adds a warning message to the current logs list."""
        self.log(message, 'WARNING')

    def save_logs(self, flush: bool = True):
        """Appends the accumulated logs to the CSV file at the specified path."""

        existing_logs = pd.read_csv(self.path).values.tolist()
        updated_logs = existing_logs + self.logs
        pd.DataFrame(updated_logs, columns=self.headers).to_csv(self.path, index=False)
        self._run_log_entry()

        if flush:    
            self.logs = []

    def __log_file_exists__(self) -> bool:
        """Checks if the log file exists at the specified path."""
        path = logging_dir + '{self.log_file_name}'
        return os.path.isfile(path)
